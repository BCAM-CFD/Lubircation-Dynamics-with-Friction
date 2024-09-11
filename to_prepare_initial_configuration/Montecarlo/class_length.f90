!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
module class_length
  !--------------------------------
  ! Class length
  !--------------------------------
  use class_computational
  IMPLICIT NONE

  TYPE length_type
     REAL(Pr) :: min, max
  END type length_type

END module class_length
