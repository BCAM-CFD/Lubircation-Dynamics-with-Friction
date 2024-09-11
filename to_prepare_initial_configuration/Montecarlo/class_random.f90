!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
module class_random
  !---------------------------------
  ! Class related to random number generator
  !---------------------------------
  use class_computational
  IMPLICIT NONE
  
  TYPE random_type

     LOGICAL :: fixed_seed
     INTEGER :: seed

     !-- From Gaussian random numbers used by Xin --
     INTEGER  :: iset
     REAL(Pr) :: gset
     
  END type random_type

  CONTAINS
    include 'inc_read_random.f90'
    include 'inc_init_random_seed.f90'
    include 'inc_random_constructor.f90'
    include 'inc_random_gaussian.f90'
    include 'inc_write_random_info.f90'

END module class_random
