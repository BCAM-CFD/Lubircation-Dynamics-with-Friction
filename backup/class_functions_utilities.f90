!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!   Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Jose Antonio Ruiz Lopez, currently from the Department of applied physics at
!   Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!   in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------

!*********************************************
!  CLASS FUNCTIONS_UTILITIES
!*********************************************
! This is not really a class, but a group of 
! utilities related to functions.
!-----------------------------------------------------

module class_functions_utilities
  use class_computational
  IMPLICIT NONE
  
  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_mean_value.f90'
    include 'inc_lin_regression.f90'
    include 'inc_bit_function.f90'
    include 'inc_data_delimiter.f90'

  END module class_functions_utilities
