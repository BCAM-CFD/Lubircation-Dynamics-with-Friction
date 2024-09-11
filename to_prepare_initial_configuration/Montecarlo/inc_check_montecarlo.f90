!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------- check_montecarlo --------
!-------------------------------------------------------
! We do some tests to be sure that the input 
! is correct.
!-------------------------------------------------------

  IF (this%N_steps_check .LE. 0) THEN
     WRITE(*,*) '*** Input error: N_steps_check should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (this%N_freq_stat .LE. 0) THEN
     WRITE(*,*) '*** Input error: N_freq_stat should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (this%amplitude > this%max_amp) THEN
     WRITE(*,*) '*** Input error: amplitude should be less or equal than max_amp. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (this%amplitude < this%min_amp) THEN
     WRITE(*,*) '*** Input error: amplitude should be greater or equal than min_amp. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (this%max_amp < this%min_amp) THEN
     WRITE(*,*) '*** Input error: max_amp should be greater than min_amp. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- max_amplitude --
  IF (this%max_amp .LE. 0) THEN
     WRITE(*,*) '*** Input error: max_amp should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (this%min_amp .LE. 0) THEN
     WRITE(*,*) '*** Input error: min_amp should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- acceptance_ratio --
  IF ((this%acceptance_ratio .LE. 0) .OR. (this%acceptance_ratio .GE. 1)) THEN
     WRITE(*,*) '*** Input error: acceptance ratio should be between 0 and 1. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

!------------ end check_montecarlo --------
