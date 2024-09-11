!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
module class_particle
  !---------------------------------
  ! Class related to particles
  !---------------------------------
  use class_computational
  use class_neighbours
  IMPLICIT NONE
  
  TYPE particle_type

     !-- Basic physic variables --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos  !-- position --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: old_pos  !-- old position --
     INTEGER, DIMENSION(3)               :: cell0
     INTEGER :: Id

  END type particle_type

  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_create_particle.f90'
    include 'inc_destroy_particle.f90'
 
END module class_particle
