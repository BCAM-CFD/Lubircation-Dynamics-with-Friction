!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
 module class_sim
   !--------------------------------
   ! Class of the whole sim
   !--------------------------------
   use class_computational
   use class_montecarlo
   use class_physics
   use class_random
   use class_file
   use class_cell_system
   use class_potential
   IMPLICIT NONE

   TYPE sim_type
     !-- files --
     TYPE(file_type) , DIMENSION(:), ALLOCATABLE :: file
     CHARACTER(LEN=MAX_CHAR) :: input
     CHARACTER(LEN=MAX_CHAR) :: dir
     CHARACTER(LEN=MAX_CHAR) :: file_pos
     LOGICAL, DIMENSION(:), ALLOCATABLE :: forbidden !-- forbidden units to use --
     INTEGER :: N_save
     INTEGER :: N_save_pos
     INTEGER :: initial

     TYPE(montecarlo_type)  :: montecarlo

     TYPE(random_type)      :: random

     TYPE(physics_type)     :: physics

     TYPE(potential_type)   :: potential

     TYPE(cell_system_type) :: cells

  END type sim_type

   !-------- SUBROUTINES AND FUNCTIONS ---------------
   CONTAINS
     
     include 'inc_read_input.f90'
     include 'inc_initialize_sim.f90'
     include 'inc_initialize_files.f90'
     include 'inc_read_sim.f90'
     include 'inc_main_loop.f90'
     include 'inc_write_info.f90'
     include 'inc_write_all_files_info.f90'
     include 'inc_write_initial.f90'
     include 'inc_destroy_sim.f90'

   end module class_sim
