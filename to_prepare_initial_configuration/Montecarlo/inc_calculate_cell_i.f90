!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE Calculate_cell_i(this, part, new_cell, error_out)
  !-----------------------------------------------
  ! This subroutine calculates which cell is the
  ! particle 'part' belonging to.
  !-----------------------------------------------
  use class_particle
  IMPLICIT NONE
  TYPE(cell_system_type), INTENT(in)  :: this
  TYPE(particle_type), INTENT(inout)  :: part
  INTEGER, DIMENSION(:),  INTENT(out) :: new_cell
  INTEGER, INTENT(out)                :: error_out
  INTEGER :: dim
  INTEGER :: I, J, K

  error_out = 0

  dim = SIZE(part%pos)

  IF (dim == 2) THEN
     DO I = 1, this%Ncells(1)
        DO J = 1, this%Ncells(2)
           !--- If it is inside the x range ---
           IF ((part%pos(1) .GE. this%coord(I,J,1)%L(1)%min)  .AND. &
                (part%pos(1) .LE. this%coord(I,J,1)%L(1)%max)) THEN
              !--- and into the y range ---
              IF ((part%pos(2) .GE. this%coord(I,J,1)%L(2)%min)  &
                   .AND. &
                   (part%pos(2) .LE.this%coord(I,J,1)%L(2)%max)) THEN
                 new_cell(1) = I
                 new_cell(2) = J
                 new_cell(3) = 1
                 GOTO 997
              ENDIF
           ENDIF
        ENDDO
     ENDDO
     !-- If this line is reached i was not in any cell --
     error_out = 1
     WRITE(*,*) '*** Calculate_cell_i error: the cell which i is  '
     WRITE(*,*) '    belonging could not be found. ***'
     GOTO 1000 !-- End of subroutine --
997  CONTINUE        
  ELSE !--dim == 3 --
     DO I = 1, this%Ncells(1)
        DO J = 1, this%Ncells(2)
           DO K = 1, this%Ncells(3)
              !--- If it is inside the x range ---
              IF ((part%pos(1) .GE.this%coord(I,J,K)%L(1)%min)  &
                   .AND. (part%pos(1) .LE. &
                   this%coord(I,J,K)%L(1)%max)) THEN
                 !--- and into the y range ---
                 IF ((part%pos(2) .GE.this%coord(I,J,K)%L(2)%min)  &
                      .AND. (part%pos(2) .LE. &
                      this%coord(I,J,K)%L(2)%max)) THEN
                    !--- and into the z range ---
                    IF ((part%pos(3) .GE. this%coord(I,J,K)%L(3)%min)  &
                         .AND. (part%pos(3) .LE. &
                         this%coord(I,J,K)%L(3)%max)) THEN
                       new_cell(1) = I
                       new_cell(2) = J
                       new_cell(3) = K
                       GOTO 998                               
                    ENDIF
                 ENDIF
              ENDIF
           ENDDO
        ENDDO
     ENDDO
     !-- If this line is reached i was not in any cell --
     error_out = 1
     WRITE(*,*) '*** Calculate_cell_i error: the cell which i is  '
     WRITE(*,*) '    belonging could not be found. ***'
     WRITE(*,*) part%pos
     GOTO 1000 !-- End of subroutine --
998  CONTINUE        
  ENDIF

1000 CONTINUE

END SUBROUTINE Calculate_cell_i
