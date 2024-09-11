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
SUBROUTINE check_pos_walls(this, error_out)
  !-----------------------------------------------
  ! Checking if the particles enter into the walls
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)    :: this
  INTEGER, INTENT(out)                :: error_out
  INTEGER :: I
  INTEGER :: dim
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  
  
  error_out = 0

  file_name = 'inc_create_system.f90'  

  dim = this%dim

  DO I = 1, this%N
     IF (this%part(I)%pos(dim) .LE. this%part(I)%R) THEN
        CALL error_header(file_name)        
        WRITE(*,*) '*** Check pos walls error: the particle', I, &
             'is overlapping with the bottom wall. ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
     IF (this%L(dim) - this%part(I)%pos(dim) .LE. this%part(I)%R) THEN
        CALL error_header(file_name)        
        WRITE(*,*) '*** Check pos walls error: the particle', I, &
             'is overlapping with the top wall. ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDDO

1000 CONTINUE

END SUBROUTINE check_pos_walls
