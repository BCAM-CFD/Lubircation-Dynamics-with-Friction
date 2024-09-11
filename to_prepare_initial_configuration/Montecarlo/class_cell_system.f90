!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
module class_cell_system
  !--------------------------------
  ! Class about the cell system. In here we manage all the cells from the system.
  !--------------------------------
  use class_cell
  IMPLICIT NONE

  TYPE cell_system_type
     TYPE(cell_type), DIMENSION(:,:,:), ALLOCATABLE :: coord
     INTEGER, DIMENSION(:), ALLOCATABLE             :: Ncells
  END type cell_system_type
  
CONTAINS
  include 'inc_cell_system_constructor.f90'
  include 'inc_calculate_cells_list.f90'
  include 'inc_change_cell_i.f90'
  include 'inc_calculate_cell_i.f90'
  include 'inc_add_part_to_cell.f90'
  
  include 'inc_cell_system_destructor.f90'
  include 'inc_write_cell_system_info.f90'
  
END module class_cell_system
