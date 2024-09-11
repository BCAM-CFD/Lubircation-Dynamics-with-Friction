!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------
SUBROUTINE read_montecarlo(this, input, error_out)
!------------------------------------------------------
! Quantities of class montecarlo are read from input.
!-------------------------------------------------------
  use class_read_input
  IMPLICIT NONE
  TYPE(montecarlo_type), INTENT(inout) :: this
  CHARACTER(LEN=MAX_CHAR), INTENT(In)  :: input
  INTEGER, INTENT(out)                 :: error_out
  CHARACTER(LEN=MAX_CHAR) :: name
  
  error_out = 0

  !--------------  N_steps_check ----------------------
  name = 'N_steps_check'
  CALL read_variable(input, name, this%N_steps_check, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  Nfreq_stat ----------------------
  name = 'N_freq_stat'
  CALL read_variable(input, name, this%N_freq_stat, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  amplitude ----------------------
  name = 'amplitude'
  CALL read_variable(input, name, this%amplitude, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  max_amp ----------------------
  name = 'max_amp'
  CALL read_variable(input, name, this%max_amp, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  min_amp ----------------------
  name = 'min_amp'
  CALL read_variable(input, name, this%min_amp, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  acceptance_ratio ----------------------
  name = 'acceptance_ratio'
  CALL read_variable(input, name, this%acceptance_ratio, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF
  
  include 'inc_check_montecarlo.f90'

1000 CONTINUE

END SUBROUTINE read_montecarlo
