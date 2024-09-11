!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Sagaya Savarimuthu Prasanna Kumar, from Marco Ellero's group.
! - Jose Antonio Ruiz-Lopez, from Marco Ellero's group.
!    Currently from the Department of Applied Physics at
!    Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Sagaya Savarimuthu Prasanna Kumar.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------
module class_wall
  !---------------------------------
  ! Class related to walls 
  ! (both bottom and top walls at the box limits in z direction)
  !---------------------------------
  use class_computational
  IMPLICIT NONE
  
  TYPE wall_type

     !-- Top wall variables --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: vel_top   !-- velocity of the top wall --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: force_top !-- force on the top wall --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: lub_force_top !-- lub force on the top wall --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: rep_force_top !-- rep force on the top wall --

     !-- Bottom wall variables --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: vel_bottom   !-- velocity of the bottom wall -
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: force_bottom !-- force on the bottom wall --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: lub_force_bottom !-- lub force on the bottom wall --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: rep_force_bottom !-- rep force on the bottom wall --

  END type wall_type

  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_wall_constructor.f90'
    include 'inc_wall_destructor.f90'
    include 'inc_initial_wall.f90'
    include 'inc_write_wall_info.f90'
 
  END module class_wall
