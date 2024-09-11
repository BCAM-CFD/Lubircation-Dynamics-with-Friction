!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
module class_coord
  !--------------------------------
  ! Class coord
  !--------------------------------
  IMPLICIT NONE

  TYPE coord_type
     INTEGER, DIMENSION(:), ALLOCATABLE :: coord
  END type coord_type

  CONTAINS
    include 'inc_coord_destructor.f90'

END module class_coord
