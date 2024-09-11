!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------
SUBROUTINE read_physics(this, input, error_out)
!------------------------------------------------------
! Quantities of class physics are read from input.
!-------------------------------------------------------
  use class_read_input
  IMPLICIT NONE
  TYPE(physics_type), INTENT(inout)    :: this
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: input
  INTEGER, INTENT(out)                :: error_out
  CHARACTER(LEN=MAX_CHAR) :: name

  !--------------  Box ----------------------
  name = 'box'
  CALL read_variable(input, name, this%box, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  N_part ----------------------
  name = 'N_part'
  CALL read_variable(input, name, this%N_part, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  dim ----------------------
  name = 'dim'
  CALL read_variable(input, name, this%dim, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  kT ----------------------
  name = 'kT'
  CALL read_variable(input, name, this%kT, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  rcut ----------------------
  name = 'rcut'
  CALL read_variable(input, name, this%rcut, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  include 'inc_check_physics.f90'

1000 CONTINUE

END SUBROUTINE read_physics
