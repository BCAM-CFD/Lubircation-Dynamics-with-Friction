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
!---------------------------------------------
SUBROUTINE cells_neighbours(this)
  !---------------------------------------------
  ! The neighbour cells of every cell are calculated
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER :: I, J, K, L, M, N
  INTEGER :: L_ini, L_end
  INTEGER :: N_ini, N_end
  INTEGER :: counter
  INTEGER :: coord_x, coord_y, coord_z

  IF (this%dim == 2) THEN
     DO I = 1, this%Ncells(1)
        DO J = 1, this%Ncells(2)
           counter = 0
           L_ini = -1
           L_end =  1
           IF (J == 1) THEN !-- Bottom wall --
              L_ini = 0
              L_end = 1
           ELSE IF (J == this%Ncells(2)) THEN !-- Top wall --
              L_ini = -1
              L_end =  0
           ENDIF
              
           DO K = -1, 1
              DO L = L_ini, L_end
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
                 this%cell(I,J,1)%neigh_coord(counter,1) = coord_x
                 this%cell(I,J,1)%neigh_coord(counter,2) = coord_y
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ELSE !-- dim == 3 --
     DO I = 1, this%Ncells(1)
        DO J = 1, this%Ncells(2)
           DO M = 1, this%Ncells(3)
              counter = 0
              N_ini = -1
              N_end =  1
              IF (M == 1) THEN !-- Bottom wall --
                 N_ini = 0
                 N_end = 1
              ELSE IF (M == this%Ncells(3)) THEN !-- Top wall --
                 N_ini = -1
                 N_end =  0
              ENDIF
              DO K = -1, 1
                 DO L = -1, 1
                    DO N = N_ini, N_end
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
                       this%cell(I,J,M)%neigh_coord(counter,1) = coord_x
                       this%cell(I,J,M)%neigh_coord(counter,2) = coord_y
                       this%cell(I,J,M)%neigh_coord(counter,3) = coord_z
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ENDIF

END SUBROUTINE cells_neighbours
