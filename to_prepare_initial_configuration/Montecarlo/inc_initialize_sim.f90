!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------------
  SUBROUTINE initialize_sim(this, error_out)
!---------------------------------------------------
    ! The simulation is initialized
    !-----------------------------------------------
    IMPLICIT NONE
    TYPE(sim_type), INTENT(inout) :: this
    INTEGER, INTENT(out)          :: error_out

    error_out = 0

    !-- The files are initialized --
    CALL initialize_files(this, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

    !-- The random object is initialized --
    CALL random_constructor(this%random, this%file(7)%unit)

    !-- The physics is initialized --
    CALL initialize_physics(this%physics, this%initial, &
         this%file(6), this%file(7)%unit, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

    !-- The potential is initialized --
    CALL initialize_potential(this%potential, this%physics%rcut, this%file(7)%unit)

    !-- The mntecarlo object is initialized --
    CALL initialize_montecarlo(this%montecarlo, this%physics%rcut, &
         this%physics%kT, this%file(7)%unit)

    !-- The cell system is constructed --
    CALL cell_system_constructor(this%cells, this%physics%box, &
         this%physics%rcut, this%physics%N_part, this%file(7)%unit, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    

    !-- The list of particles for every cell is computed --
    CALL calculate_cells_list(this%cells, this%physics%part, &
         error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

    !-- The initial energy is calculated --
!!$    CALL compute_energy_table(this%physics, this%cells, this%potential)
    CALL compute_energy(this%physics, this%cells)

1000 CONTINUE

  END SUBROUTINE initialize_sim
