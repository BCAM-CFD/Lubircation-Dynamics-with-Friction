!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!***********************************************
PROGRAM montecarlo_program
  !***********************************************
  ! A program to do a Montecarlo simulation with particles interacting
  ! with radial  potentials. If necessary,  a table for  the potential
  ! can  be used  to optimize  the  code.  To  do that,  the calls  to
  ! compute_energy*   subroutines  can   be   changed   by  calls   to
  ! compute_energy*table   subroutines.   In   order  to   change  the
  ! potential you  will need  to edit  the inc_potential.f90  file and
  ! recompile.
  !-------------------------------------------
  use class_sim
  IMPLICIT NONE
  TYPE(sim_type) :: sim
  INTEGER        :: error_out
  
  error_out = 0
  
  !-- The input is read --
  CALL read_input(sim, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF

  !-- The system is initialized --
  CALL initialize_sim(sim, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF

  !-- The initial state and some more information is written out --
  CALL write_initial(sim, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF

!-------------------- Main loop ------------------------
  CALL main_loop(sim, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF
!-------------------------------------------------------

1000 CONTINUE
  
  CALL destroy_sim(sim)

  WRITE(*,*) '***********************'
  WRITE(*,*) '** END OF SIMULATION **'
  WRITE(*,*) '***********************'

END PROGRAM montecarlo_program
