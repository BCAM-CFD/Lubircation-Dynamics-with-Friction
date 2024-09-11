!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
 module class_physics
   !--------------------------------
   ! Class of the whole sim
   !--------------------------------
   use class_computational
   use class_particle
   IMPLICIT NONE

   TYPE physics_type
      REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box
      INTEGER  :: N_part
      INTEGER  :: dim
      REAL(Pr) :: kT
      TYPE(particle_type), DIMENSION(:), ALLOCATABLE :: part
      REAL(Pr) :: rcut
      REAL(Pr) :: rcut_sq
      REAL(Pr) :: energy
      
  END type physics_type

   !-------- SUBROUTINES AND FUNCTIONS ---------------
   CONTAINS
     
     include 'inc_read_physics.f90'
     include 'inc_initialize_physics.f90'
     include 'inc_initialize_positions.f90'
     include 'inc_write_pos.f90'
     include 'inc_write_energy.f90'
     include 'inc_write_physics_info.f90'
     include 'inc_read_pos.f90'
     include 'inc_compute_energy.f90'
     include 'inc_compute_energy_table.f90'
     include 'inc_compute_energy_i.f90'
     include 'inc_compute_energy_i_table.f90'
     include 'inc_check_compute_energy.f90'
     include 'inc_check_compute_energy_table.f90'
     include 'inc_change_pos.f90'
     include 'inc_destroy_physics.f90'

   end module class_physics
