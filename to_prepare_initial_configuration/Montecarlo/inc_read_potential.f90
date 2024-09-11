!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------
SUBROUTINE read_potential(this, input, error_out)
!------------------------------------------------------
! Quantities of class potential are read from input.
!-------------------------------------------------------
  use class_read_input
  IMPLICIT NONE
  TYPE(potential_type), INTENT(inout) :: this
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: input
  INTEGER, INTENT(out)                :: error_out
  CHARACTER(LEN=MAX_CHAR) :: name

  !--------------  Ntable ----------------------
  name = 'Ntable'
  CALL read_variable(input, name, this%Ntable, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF


  include 'inc_check_potential.f90'

1000 CONTINUE

END SUBROUTINE read_potential
