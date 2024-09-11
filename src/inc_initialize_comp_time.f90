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
!------------------------------------------
SUBROUTINE initialize_comp_time(this)
  !------------------------------------------
  ! Initializing conputational times.
  !------------------------------------------
  IMPLICIT NONE
  TYPE(comp_time_type) :: this

  this%total     = 0.0_Pr
  this%neigh     = 0.0_Pr
  this%semi_impl = 0.0_Pr
  this%VV        = 0.0_Pr

END SUBROUTINE initialize_comp_time
