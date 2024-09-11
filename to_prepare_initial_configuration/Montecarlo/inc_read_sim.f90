!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------
SUBROUTINE read_sim(this, error_out)
!------------------------------------------------------
! Quantities of class sim are read from input.
!-------------------------------------------------------
  use class_read_input
  IMPLICIT NONE
  TYPE(sim_type), INTENT(inout) :: this
  INTEGER, INTENT(out)          :: error_out
  CHARACTER(LEN=MAX_CHAR) :: name
  
  error_out = 0

  !--------------  dir ----------------------
  name = 'dir'
  CALL read_variable(this%input, name, this%dir, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  N_save ----------------------
  name = 'N_save'
  CALL read_variable(this%input, name, this%N_save, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  N_save_pos ----------------------
  name = 'N_save_pos'
  CALL read_variable(this%input, name, this%N_save_pos, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  initial ----------------------
  name = 'initial'
  CALL read_variable(this%input, name, this%initial, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  file_pos ----------------------
  name = 'file_pos'
  CALL read_variable(this%input, name, this%file_pos, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  include 'inc_check_sim.f90'

 1000 CONTINUE

END SUBROUTINE read_sim
