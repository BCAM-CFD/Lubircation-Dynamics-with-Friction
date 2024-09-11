!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Sagaya Savarimuthu Prasanna Kumar, from Marco Ellero's group.
! - Jose Antonio Ruiz-Lopez, from Marco Ellero's group.
!    Currently from the Department of Applied Physics at
!    Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Sagaya Savarimuthu Prasanna Kumar.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------
!-----------------------------------------------
SUBROUTINE initial_cells(this, R, error_out)
  !-----------------------------------------------
  ! The cells for searching neighbours are created
  ! and initialized
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  REAL(Pr), INTENT(in)             :: R
  INTEGER, INTENT(out)             :: error_out
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: L_min, L_max
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: cell_length
  REAL(Pr) :: ai
  REAL(Pr) :: Vcell
  REAL(Pr) :: Vpart
  REAL(Pr) :: extra_mem
  INTEGER :: I, J, K
  REAL(Pr) :: pi
  LOGICAL :: limit_wall
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --    
  
  error_out = 0

  file_name = 'inc_initial_cells.f90'
  
  pi = 4.0_Pr * ATAN(1.0_Pr)

  ALLOCATE(cell_length(this%dim))

  ALLOCATE(L_min(this%dim))
  ALLOCATE(L_max(this%dim))
  L_min(:) = 0.0_Pr
  L_max(:) = this%L(:)

  this%Ncells(1:this%dim) = INT((L_max(1:this%dim) - L_min(1:this%dim)) / this%Rmax)
  IF (this%dim == 2) THEN
     this%Ncells(3) = 1
  ENDIF
  DO I = 1, this%dim
     IF (this%Ncells(I) <= 2) THEN
        CALL error_header(file_name)                
        error_out = 1
        WRITE(*,*) '*** Initial cells error: the program needs at least three cells in each direction.'
        WRITE(*,*) 'Take a smaller rcut or rlist, or a bigger system. ***'
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDDO

  !--- Length of the cells is computed ---
  cell_length(1:this%dim) = (L_max(1:this%dim) - L_min(1:this%dim)) / this%Ncells(1:this%dim)

  !-- Cell and total volumes are calculated --
  Vpart= 0.0_Pr
  
  DO I= 1, this%N
    ai = this%part(I)%R
    Vpart = Vpart + ai * ai * ai
  END DO
  Vpart=Vpart/this%N
  Vpart = 4.0_Pr / 3.0_Pr * pi * Vpart
  Vcell = 1.0_Pr
  DO I = 1, this%dim
     Vcell = Vcell * cell_length(I)
  ENDDO
  
  !--- Memory for the cells is allocated ---
  extra_mem = 3.0_Pr !-- This should be changed if needed --
  this%Npart_max = NINT(extra_mem * Vcell / Vpart)
  IF (this%Npart_max .LE. 1) THEN
     CALL error_header(file_name)             
     WRITE(*,*) '*** initial cells error: Npart_max is too small. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF
  allocate(this%cell(this%Ncells(1), this%Ncells(2), this%Ncells(3)))
  DO I = 1, this%Ncells(1)
     DO J = 1, this%Ncells(2)
        DO K = 1, this%Ncells(3)
           limit_wall = .FALSE.
           IF ((K == 1) .OR. (K == this%Ncells(3))) THEN
              limit_wall = .TRUE.
           ENDIF
              
           CALL cell_constructor(this%cell(I,J,K), this%dim, &
                this%Npart_max, limit_wall, error_out)
           IF (error_out .NE. 0) THEN
              GOTO 1000 !-- End of subroutine --
           ENDIF
        ENDDO
     ENDDO
  ENDDO

  !--- Edges of the cells are calculated ---
  CALL cells_edges(this, cell_length, L_min)

  !--- Cell's neighbours are calculated ----
  CALL cells_neighbours(this)
  
1000 CONTINUE  

  IF (ALLOCATED(L_min)) THEN
     DEALLOCATE(L_min)
  ENDIF
  IF (ALLOCATED(L_max)) THEN
     DEALLOCATE(L_max)
  ENDIF
  IF (ALLOCATED(cell_length)) THEN
     DEALLOCATE(cell_length)
  ENDIF

END SUBROUTINE initial_cells
