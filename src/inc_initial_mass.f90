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
!------------------------------------------------
SUBROUTINE initial_mass(this, m)
  !------------------------------------------------
  ! The mass is assigned to the particles.
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  REAL(Pr), INTENT(in)             :: m

  this%part(:)%mass = m * this%part(:)%R*this%part(:)%R*this%part(:)%R
  
END SUBROUTINE initial_mass
