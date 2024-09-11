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

!------------------------------------------------------
PROGRAM biviscous_suspension
  !------------------------------------------------------
  ! Code  for  simulating  suspensions  of spherical  particles  in  a
  ! fluid. The hydrodynamic interactions are modeled using lubrication
  ! and friction forces, so the code  is expected to be valid only for
  ! concentrated systems. Particle spins are also considered
  !  References:
  !   - José  Antonio Ruiz-Lopez,  Sagaya Savarimuthu  Prasanna Kumar,
  !      Adolfo    Vazquez-Quesada,    Juan     de    Vicente,    Marco
  !      Ellero. Tribological variable-friction  coefficient models for
  !      the  simulation of  dense  suspensions  of rough  polydisperse
  !      particles. Journal of Rheology, 67(2), 541-558. 2023
  !   - Sagaya  Savarimuthu  Prasanna Kumar;  Adolfo  Vázquez-Quesada;
  !      Marco Ellero.  A conservative lubrication dynamics  method for
  !      the  simulation   of  dense  non-colloidal   suspensions  with
  !      particle   spin.  Journal   of  Computational   Physics.  427,
  !      pp. 110001, 2021.
  !
  !  The code is written in an Object Oriented
  ! Programming style (see Introduction to Object-Oriented Concepts
  ! using Fortran 90.  by V.K. Decyk, C.D. Norton and B.K.Szymanski
  ! for more details. To know how to compile and use the program,
  ! check the documentation.
  ! --------------------------------------------------
  use class_system
  use class_input
  use class_comp_time
  IMPLICIT NONE
  TYPE(system_type)    :: sim   !-- system object --
  TYPE(input_type)     :: input !-- input object --
  TYPE(comp_time_type) :: time  !-- Computational time object --
  INTEGER :: error_out          !-- To handle errors --
  INTEGER :: step               !-- time step --
  LOGICAL :: first_step         !-- To know if its the 1st step --
  CHARACTER(LEN=MAX_CHAR) :: input_file !-- The input file --
  REAL :: t0, tf               !-- To measure total computational time
  REAL :: t0_loop, tf_loop     !-- To measure loop computational time

  CALL cpu_time(t0)

  error_out = 0

  !---- The input is read ----
  input_file = 'input'
  CALL read_input(input, input_file, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF

  !---- System is created and initialized ----
  CALL create_system(sim, input,  error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF

  !-- We do not need the input object anymore --
  CALL input_destructor(input)

  !-- The input and the paticles files are copied in the output directory --
  CALL SYSTEM('cp '//trim(input_file)//' '//trim(sim%dir_output))
  IF (sim%read_pos) THEN
     CALL SYSTEM('cp '//trim(sim%file_pos)//' '//trim(sim%dir_output))
  ENDIF
  IF (sim%read_vel) THEN
     CALL SYSTEM('cp '//trim(sim%file_vel)//' '//trim(sim%dir_output))
  ENDIF

  !---- System data is outputted ----
  CALL write_info(sim, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF

  !-- The initial data is outputted ---
  step = 0
  CALL write_particles(sim, step, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF

  !-- The initial neighbour list is calculated --
  CALL initial_neigh_list(sim, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF
  first_step = .TRUE.

  !-- Checking distances --
  CALL check_part_dist(sim, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF

  !************** Main loop ***********************
  CALL initialize_comp_time(time)
  WRITE(*,*) '*** The simulation starts ***'
  WRITE(*,*) sim%Bp
  CALL cpu_time(t0_loop)
  DO step = 1, sim%Nsteps

     !-- Neighbours are calculated --
     CALL calculate_neigh(sim, time, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of program --
     ENDIF

     IF (.NOT.(sim%biviscous)) THEN
        !------------ Monoviscous case --------------
        IF (.NOT.(sim%explicit)) THEN
           !-- Implicit method to calculate velocities 
           !   considering only lubrication forces is done --
           CALL vel_iterative(sim, time, error_out)
           IF (error_out .NE. 0) THEN
              GOTO 1000 !-- End of program --
           ENDIF

           !-- The repulsion force is considered with a velocity
           !   Verlet integrator --
           CALL VV(sim, first_step, time, error_out)
           IF (error_out .NE. 0) THEN
              GOTO 1000 !-- End of program --
           ENDIF

        ELSE
           CALL VV_explicit(sim, first_step, error_out)
           IF (error_out .NE. 0) THEN
              GOTO 1000 !-- End of program --
           ENDIF
        ENDIF
     ELSE 
        !----------- Biviscous case -----------
        IF (.NOT.(sim%explicit)) THEN
           !-- Implicit method to calculate velocities 
           !   considering only lubrication forces is done --
           !WRITE(*,*) '*** bi-viscous ***'
           CALL vel_iterative_biviscous(sim,time, error_out)
           IF (error_out .NE. 0) THEN
              GOTO 1000 !-- End of program --
           ENDIF

           !-- The repulsion force is considered with a velocity
           !   Verlet integrator --
           CALL VV(sim, first_step, time, error_out)
           IF (error_out .NE. 0) THEN
              GOTO 1000 !-- End of program --
           ENDIF
        ELSE
           error_out = 1
           WRITE(*,*) '*** Main error: the explicit integrator for'
           WRITE(*,*) '    the biviscous case has not been &
                programmed. ***'
           GOTO 1000 !-- End of program --
        ENDIF
     ENDIF

		
     !-- The results are written --
     IF (MOD(step, sim%output%freq_write) == 0) THEN
        
        CALL cpu_time(tf_loop)
        time%total = tf_loop - t0_loop
        CALL write_comp_times(time, step, sim%output%comp_time%unit)
        !-- The particles from the bulk are found --
        CALL bulk(sim)
        CALL compute_shear_rate(sim, error_out)
        IF (error_out .NE. 0) THEN
           GOTO 1000 !-- End of program --
        ENDIF
        CALL stress(sim, error_out)
        IF (error_out .NE. 0) THEN
           GOTO 1000 !-- End of program --
        ENDIF
        WRITE(*,*) 'Step', step
        CALL write_wall(sim, step)
        CALL write_stress(sim, step)
        CALL calc_partShear(sim, error_out)
        CALL write_shear_rate(sim, step)
        CALL write_Nsweeps(sim, step)
        CALL write_neighbours(sim, step)
!        CALL flush() !-- To write out immediatly --
     ENDIF
     IF (MOD(step, sim%output%freq_write_part) == 0) THEN
        !-- The particles from the bulk are found --
        CALL bulk(sim)

        CALL write_particles(sim, step, error_out)
        IF (error_out .NE. 0) THEN
           GOTO 1000 !-- End of program --
        ENDIF
     ENDIF

     first_step = .FALSE.
  ENDDO
  WRITE(*,*) '*** The simulation is ending up ***'

1000 CONTINUE

     !--- Objects are destroyed in order to release memory ---
     CALL destroy_system(sim)

     CALL cpu_time(tf)
     WRITE(*,*) '*** Total time of simulation', tf - t0,'secs ***'

     WRITE(*,*) '**************************'
     WRITE(*,*) '***** END OF PROGRAM *****'
     WRITE(*,*) '**************************'

   END PROGRAM biviscous_suspension
