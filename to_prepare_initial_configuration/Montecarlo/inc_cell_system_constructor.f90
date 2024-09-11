!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------------
SUBROUTINE cell_system_constructor(this, box, Rmax, N_T, unit, error_out)
  !------------------------------------------------------------------
  ! Constructor of the class cell_system
  !------------------------------------------------------------------
  IMPLICIT NONE
  TYPE(cell_system_type), INTENT(inout) :: this
  REAL(Pr), DIMENSION(:), INTENT(in)    :: box
  REAL(Pr), INTENT(in)                  :: Rmax
  INTEGER, INTENT(in)                   :: N_T
  INTEGER, INTENT(in)                   :: unit
  INTEGER, INTENT(out)                  :: error_out
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: cells_length
  INTEGER  :: dim
  INTEGER  :: I, J, K, L, M, N
  INTEGER  :: coord_x, coord_y, coord_z
  INTEGER  :: counter
  INTEGER  :: N_part
  REAL(Pr) :: Lx, Ly, Lz
  REAL(Pr) :: Lx0, Ly0, Lz0

  error_out = 0

  dim = SIZE(box)
  
  !-- Some needed allocations are made --
  ALLOCATE(cells_length(dim))
  
  !--- The number of cells in every direction is determined ---
  ALLOCATE(this%Ncells(3))
  this%Ncells(1:dim) = INT(box(1:dim) / Rmax)
  IF (dim == 2) THEN
     this%Ncells(3) = 1
  ENDIF

  IF (MINVAL(this%Ncells(1:dim)) .LE. 2) THEN
     WRITE(*,*)  '*** Cell system constructor error: there is not enough cells of'
     WRITE(*,*) 'simulation. This could be caused becuase: '
     WRITE(*,*) ' 1.- You have a very small number of particles. '
     WRITE(*,*) ' 2.- The rcut you are using is very big.  ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--- The length of the cells is computed ---
  cells_length(1:dim) = box(1:dim) / this%Ncells(1:dim)
  
  !--- The memory for the cells is allocated ---
  allocate(this%coord(this%Ncells(1), this%Ncells(2), this%Ncells(3)))
  DO I = 1, this%Ncells(1)
     DO J = 1, this%Ncells(2)
        DO K = 1, this%Ncells(3)
           allocate(this%coord(I,J,K)%L(dim))
        ENDDO
     ENDDO
  ENDDO

  !--- The edges of the cells are calculated ---
  IF (dim == 2) THEN
     Lx0 = 0.0_Pr
     DO I = 1, this%Ncells(1)
        Lx = Lx0 + cells_length(1)
        Ly0 = 0.0_Pr
        DO J = 1, this%Ncells(2)
           Ly = Ly0 + cells_length(2)
           this%coord(I,J,1)%L(1)%min = Lx0
           this%coord(I,J,1)%L(1)%max = Lx
           this%coord(I,J,1)%L(2)%min = Ly0
           this%coord(I,J,1)%L(2)%max = Ly
           Ly0 = Ly
        ENDDO
        Lx0 = Lx
     ENDDO
  ELSE !-- dim == 3 --
     Lx0 = 0.0_Pr
     DO I = 1, this%Ncells(1)
        Lx = Lx0 + cells_length(1)
        Ly0 = 0.0_Pr
        DO J = 1, this%Ncells(2)
           Ly = Ly0 + cells_length(2)
           Lz0 = 0.0_Pr
           DO K = 1, this%Ncells(3)
              Lz = Lz0 + cells_length(3)
              this%coord(I,J,K)%L(1)%min = Lx0
              this%coord(I,J,K)%L(1)%max = Lx
              this%coord(I,J,K)%L(2)%min = Ly0
              this%coord(I,J,K)%L(2)%max = Ly
              this%coord(I,J,K)%L(3)%min = Lz0
              this%coord(I,J,K)%L(3)%max = Lz
              Lz0 = Lz
           ENDDO
           Ly0 = Ly
        ENDDO
        Lx0 = Lx
     ENDDO
  ENDIF

  !--- The cell's neighbours are calculated ----
  DO I = 1, this%Ncells(1)
     DO J = 1, this%Ncells(2)
        DO K = 1, this%Ncells(3)
           IF (dim == 2) THEN !every cell is neighbour of itself
              this%coord(I,J,K)%N_neigh = 9 
           ELSE
              this%coord(I,J,K)%N_neigh = 27
           ENDIF
           allocate(this%coord(I,J,K)%neigh(this%coord(I,J,K)%N_neigh))
           DO L = 1, this%coord(I,J,K)%N_neigh
              allocate(this%coord(I,J,K)%neigh(L)%coord(dim))
           ENDDO
        ENDDO
     ENDDO
  ENDDO
  IF (dim == 2) THEN
     DO I = 1, this%Ncells(1)
        DO J = 1, this%Ncells(2)
           counter = 0
           DO K = -1, 1
              DO L = -1, 1
                 coord_x = I + K
                 IF (coord_x .GT. this%Ncells(1)) THEN
                    coord_x = coord_x - this%Ncells(1)
                 ELSE IF (coord_x .LT. 1) THEN
                    coord_x = coord_x + this%Ncells(1)
                 ENDIF

                 coord_y = J + L
                 IF (coord_y .GT. this%Ncells(2)) THEN
                    coord_y = coord_y - this%Ncells(2)
                 ELSE IF (coord_y .LT. 1) THEN
                    coord_y = coord_y + this%Ncells(2)
                 ENDIF
                 counter = counter + 1
                 this%coord(I,J,1)%neigh(counter)%coord(1) = coord_x
                 this%coord(I,J,1)%neigh(counter)%coord(2) = coord_y
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ELSE 
     DO I = 1, this%Ncells(1)
        DO J = 1, this%Ncells(2)
           DO M = 1, this%Ncells(3)
              counter = 0
              DO K = -1, 1
                 DO L = -1, 1
                    DO N = -1, 1
                       coord_x = I + K
                       IF (coord_x .GT. this%Ncells(1)) THEN
                          coord_x = coord_x - this%Ncells(1)
                       ELSE IF (coord_x .LT. 1) THEN
                          coord_x = coord_x + this%Ncells(1)
                       ENDIF

                       coord_y = J + L
                       IF (coord_y .GT. this%Ncells(2)) THEN
                          coord_y = coord_y - this%Ncells(2)
                       ELSE IF (coord_y .LT. 1) THEN
                          coord_y = coord_y + this%Ncells(2)
                       ENDIF

                       coord_z = M + N
                       IF (coord_z .GT. this%Ncells(3)) THEN
                          coord_z = coord_z - this%Ncells(3)
                       ELSE IF (coord_z .LT. 1) THEN
                          coord_z = coord_z + this%Ncells(3)
                       ENDIF

                       counter = counter + 1
                       this%coord(I,J,M)%neigh(counter)%coord(1) = coord_x
                       this%coord(I,J,M)%neigh(counter)%coord(2) = coord_y
                       this%coord(I,J,M)%neigh(counter)%coord(3) = coord_z
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ENDIF

  !---- The list array for the particles inside in a cell is incialized ----
  N_part = INT(N_T / (this%Ncells(1) * this%Ncells(2) * this%Ncells(3)))
  IF (N_part > 3) THEN
     N_part = 5 * N_part !to be sure we have enough storage
  ELSE
     N_part = N_T
  ENDIF

  DO I = 1, this%Ncells(1)
     DO J = 1, this%Ncells(2)
        DO K = 1, this%Ncells(3)
           allocate(this%coord(I,J,K)%list_part(N_part))
        ENDDO
     ENDDO
  ENDDO

  WRITE(unit,*) '*** Cell system initialized ***'

1000 CONTINUE
  
  IF (ALLOCATED(cells_length)) THEN
     DEALLOCATE(cells_length)
  ENDIF

END SUBROUTINE cell_system_constructor
