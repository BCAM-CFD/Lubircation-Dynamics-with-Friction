!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------------------------
SUBROUTINE main_loop(this, error_out)
!------------------------------------------------------------------------------
  ! The main loop of the montecarlo simulation
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
  TYPE(sim_type), INTENT(inout) :: this
  INTEGER, INTENT(out)          :: error_out
  INTEGER :: I
  INTEGER :: N_tried
  INTEGER :: N_accepted
  LOGICAL :: control_end
  INTEGER :: counter
  REAL(Pr) :: mean_energy
  REAL(Pr) :: mean_energy_old
  LOGICAL :: first_time
  REAL(Pr) :: random
  INTEGER :: part_to_move
  REAL(Pr) :: initial_energy
  REAL(Pr) :: final_energy
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: old_pos
  INTEGER, DIMENSION(:), ALLOCATABLE  :: old_cell
  INTEGER, DIMENSION(:), ALLOCATABLE  :: new_cell
  LOGICAL :: changed
  REAL(Pr) :: V_nm
  LOGICAL :: accept

  error_out = 0

  ALLOCATE(old_pos(this%physics%dim))
  ALLOCATE(old_cell(this%physics%dim))
  ALLOCATE(new_cell(this%physics%dim))
  N_accepted  = 0
  N_tried     = 0
  control_end = .FALSE.
  I           = 0
  counter     = 0
  mean_energy = 0.0_Pr
  first_time  = .TRUE.
  DO WHILE(.NOT.(control_end))
     I = I + 1

     !-- A particle is selected randomly to move it --
     CALL RANDOM_NUMBER(random)
     part_to_move = 1.0_Pr + int(random * this%physics%N_part)

     !---------- OLD POSITION --------------
     !-- The energy due to i in the old position is computed --
!!$     CALL Compute_energy_i_table(this%physics, this%cells, &
!!$          part_to_move, this%potential, initial_energy)
     CALL Compute_energy_i(this%physics, this%cells, &
          part_to_move, initial_energy)

     !---------- NEW POSITION --------------
     !-- A new position is given to i --
     old_pos(:) = this%physics%part(part_to_move)%pos(:)
     CALL change_pos(this%physics, part_to_move, this%montecarlo%amplitude) 
    
     !-- We change the particle of cell if it is needed  --
     old_cell = this%physics%part(part_to_move)%cell0
     CALL change_cell_i(this%cells, this%physics%part(part_to_move), &
          new_cell, old_cell, error_out) 
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF

     !-- The energy due to i in the new position is computed --
!!$     CALL Compute_energy_i_table(this%physics, this%cells, &
!!$          part_to_move, this%potential, final_energy) 
     CALL Compute_energy_i(this%physics, this%cells, &
          part_to_move, final_energy) 
     
     !---------- ACCEPT OR NOT ACCEPT, THAT IS THE QUESTION ---------
     V_nm = final_energy - initial_energy
     !-- Decide to accept or not --
     CALL check_accept(this%montecarlo, V_nm, accept) 

     IF (accept) THEN !-- If accept the change remains --
        N_accepted = N_accepted + 1
!!$!!!$        total_energy = total_energy + V_nm
     ELSE   !-- If not, undo the i position --
        this%physics%part(part_to_move)%pos(:)  = old_pos(:)
        IF (.NOT.((new_cell(1) == old_cell(1)) .AND. &
             (new_cell(2) == old_cell(2))      .AND. &
             (new_cell(3) == old_cell(3)))) THEN
           this%physics%part(part_to_move)%cell0 = old_cell
           CALL add_part_to_cell(this%cells, old_cell, &
                this%physics%part(part_to_move), error_out)
           IF (error_out .NE. 0) THEN
              GOTO 1000 !-- End of subroutine --
           ENDIF
           CALL remove_part_from_cell(this%cells%coord(new_cell(1), &
                new_cell(2), new_cell(3)), part_to_move, error_out)
        ENDIF
        IF (error_out .NE. 0) THEN
           GOTO 1000 !-- End of subroutine --
        ENDIF
     ENDIF
     N_tried = N_tried + 1

     IF (MOD(I, this%montecarlo%N_freq_stat) == 0) THEN
        !-- The amplitude is changed if needed --
        CALL change_amp(this%montecarlo, N_accepted, N_tried)
        !-- Some output is written --
        CALL Write_montecarlo(this%montecarlo, this%file(4), I, &
             error_out) 
        IF (error_out .NE. 0) THEN
           GOTO 1000 !-- End of subroutine --
        ENDIF
     ENDIF

     !-- Write output --
     IF (MOD(I, this%N_save_pos) == 0) THEN
        CALL Write_pos(this%physics, this%file(1), I) 
     ENDIF
     IF (MOD(I, this%N_save) == 0) THEN
        WRITE(this%file(7)%unit,*) 'Step       = ', I

        !-- The total energy is computed to output it --
!!$        CALL compute_energy_table(this%physics, this%cells, this%potential) 
        CALL compute_energy(this%physics, this%cells) 
        CALL write_energy(this%physics%energy, this%file(2), &
             I, error_out)
        IF (error_out .NE. 0) THEN
           GOTO 1000 !-- End of subroutine --
        ENDIF

        counter = counter + 1
        mean_energy = mean_energy + this%physics%energy
     ENDIF

     !-- It is checked if the simulation should stop --
     IF (MOD(I, this%montecarlo%N_steps_check) == 0) THEN
        IF (first_time) THEN
           mean_energy_old = mean_energy / FLOAT(counter)
           counter = 0
           mean_energy = 0.0_Pr
           first_time = .FALSE.
        ELSE
           mean_energy = mean_energy / FLOAT(counter)
           WRITE(this%file(7)%unit,*) 'old mean energy = ',mean_energy_old
           WRITE(this%file(7)%unit,*) 'mean energy     = ',mean_energy
           IF (mean_energy > mean_energy_old) THEN
              control_end = .TRUE. !-- the simulation stops --
           ELSE
              mean_energy_old = mean_energy
              mean_energy     = 0.0_Pr
              counter         = 0
           ENDIF
        ENDIF
     ENDIF
     
  ENDDO

1000 CONTINUE

  IF (.NOT.(ALLOCATED(old_pos))) THEN
     DEALLOCATE(old_pos)
  ENDIF

  IF (.NOT.(ALLOCATED(old_cell))) THEN
     DEALLOCATE(old_cell)
  ENDIF

  IF (.NOT.(ALLOCATED(new_cell))) THEN
     DEALLOCATE(new_cell)
  ENDIF

END SUBROUTINE main_loop
