!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
  !--------------------------------
  SUBROUTINE read_input(this, error_out)
  !------------------------------
    !-------------------------------------------
    ! This subroutine reads input file and
    ! gives values to the corresponding variables.
    !-------------------------------------------
    IMPLICIT NONE
    TYPE(sim_type), INTENT(inout) :: this
    INTEGER, INTENT(out)          :: error_out
    
    !-- error_out is initialized --
    error_out = 0
    
    this%input = 'input'

    CALL read_sim(this, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

    CALL read_montecarlo(this%montecarlo, this%input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

    CALL read_random(this%random, this%input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

    CALL read_physics(this%physics, this%input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

    CALL read_potential(this%potential, this%input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

    WRITE(*,*) '*** Input read ***'

1000 CONTINUE !-- All errors come directly here --
    
  END SUBROUTINE read_input
