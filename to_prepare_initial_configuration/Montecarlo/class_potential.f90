!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
module class_potential
  !---------------------------------
  ! Class related to the potential we are using, that is going
  ! to be storaged in an array.
  !---------------------------------
  use class_computational
  IMPLICIT NONE
  
  TYPE potential_type
     
     REAL(Pr) :: rcut
     REAL(Pr) :: aux
     
     !-- Tabuled functions --
     INTEGER :: Ntable
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: val

  END type potential_type

  CONTAINS
    include 'inc_read_potential.f90'
    include 'inc_initialize_potential.f90'
    include 'inc_build_table.f90'
    include 'inc_potential.f90'
    include 'inc_potential_table.f90'
    include 'inc_write_potential.f90'
    include 'inc_write_potential_table.f90'

    include 'inc_potential_destructor.f90'
    include 'inc_write_potential_info.f90'

  END module class_potential
