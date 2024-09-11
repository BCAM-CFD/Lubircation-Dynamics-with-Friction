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
SUBROUTINE calc_mu(this, Ubar, Wbar, SRR, mu)
  !------------------------------------------------
  ! The friction coefficient is calculated
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER  :: error_out
  REAL(Pr) :: mu, mu_EHL, mu_poi, mu_cou
  REAL(Pr) :: Ubar, Wbar, SRR
  REAL(Pr) :: m
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --
  
  file_name = 'inc_friction_coeff.f90'

  m = 2.7_Pr
  
  IF (Ubar < 0) THEN

  CALL error_header(file_name)
  WRITE(*,*) '*** calc friction error: Ubar'
  WRITE(*,*) '    is negative ***'
  error_out = 1
  GOTO 1000 !-- End of subroutine --
  
  END IF
  
  IF (Wbar < 0) THEN
  
  CALL error_header(file_name)
  WRITE(*,*) '*** calc friction error: Wbar'
  WRITE(*,*) '    is negative ***'
  error_out = 1
  GOTO 1000 !-- End of subroutine --

  END IF
  


  mu_poi = 1.46_Pr*(Ubar**(0.65_Pr))*(Wbar**(-0.70_Pr))
  mu_cou = SRR*(3.8_Pr*(Ubar**(0.71_Pr))*(Wbar**(-0.78_Pr)) +&
  & 0.96_Pr*(Ubar**(0.36_Pr))*(Wbar**(-0.11_Pr)))
  
  mu_EHL = mu_poi + mu_cou
  
  mu = mu_EHL + (this%mus - mu_EHL)/(1.0_Pr + (Ubar/Wbar/this%Bp)**(m))



  
1000  CONTINUE
  
  
  
END SUBROUTINE calc_mu
