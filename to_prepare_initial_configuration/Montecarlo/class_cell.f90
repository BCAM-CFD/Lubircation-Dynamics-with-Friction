!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
module class_cell
  !--------------------------------
  ! Class about cells. Cells are used to
  ! divide the space in smaller entities.
  !--------------------------------
  use class_computational
  use class_length
  use class_coord
  IMPLICIT NONE

  TYPE cell_type

     TYPE(length_type), DIMENSION(:), ALLOCATABLE :: L
     TYPE(coord_type), DIMENSION(:), ALLOCATABLE  :: neigh
     INTEGER                                      :: N_neigh
     INTEGER                                      :: N_part
     INTEGER, DIMENSION(:), ALLOCATABLE           :: list_part
     
  END type Cell_type

  CONTAINS

    include 'inc_remove_part_from_cell.f90'
    include 'inc_check_part_cell.f90'
    include 'inc_cell_destructor.f90'
  
END module class_cell
