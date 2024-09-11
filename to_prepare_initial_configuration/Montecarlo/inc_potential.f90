!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------------------------
!  If you want to change the interparticle potential this is your file.
!  Before write a potential, check the potentials written in this file.
!-----------------------------------------------------------------

!!$!------------------------------------------------------------
!!$FUNCTION potential(R)
!!$!------------------------------------------------------------
!!$  ! Function that gives the potential energy for a given
!!$  ! distance R. If you want to change the potential between
!!$  ! particles, this is your file.
!!$  !----------------------------------------------------------
!!$  IMPLICIT NONE
!!$  REAL(Pr)             :: potential
!!$  REAL(Pr), INTENT(In) :: R
!!$  
!!$  potential = R
!!$
!!$END FUNCTION potential

!-------------------------------------------------------
FUNCTION potential(x)
  !----------------------------------
  ! This subroutine initializes the
  ! Lennard-Jones potential. In the old
  ! code there is a long range correction,
  ! to correct the effect of not taking the 
  ! potential until x -> infinity. If you need it check it.
  ! LJ potential is positive from 0 to sigma. For x > sigma
  ! the potential become negative, and tend to zero when
  ! x goes to infinity.
  !----------------------------------
  IMPLICIT NONE
  REAL(Pr)             :: potential
  REAL(Pr), INTENT(in) :: x
  INTEGER  :: I
  REAL(Pr) :: sigma
  REAL(Pr) :: epsilon

  !-- The number pi 
  sigma   = 2.05_Pr
  epsilon = 1.0_Pr

  potential =  4.0_Pr * epsilon * &
       ((sigma/x)**12.0_Pr - (sigma/x)**6.0_Pr)
  
END FUNCTION potential

!-------------------------------------------------------
FUNCTION potential_AO_soft(x)
  !----------------------------------
  ! This subroutine initializes the
  ! Asakura-Oosawa + soft repulsive potential
  !----------------------------------
  IMPLICIT NONE
  REAL(Pr)             :: potential_AO_soft
  REAL(Pr), INTENT(in) :: x
  REAL(Pr) :: n_pot
  REAL(Pr) :: sigma_c
  REAL(Pr) :: sigma_p
  REAL(Pr) :: phi_p
  REAL(Pr) :: alpha3
  REAL(Pr) :: alpha1
  REAL(Pr) :: pi, pos0, aux, zeta, alpha2
  REAL(Pr) :: b, c, join_point, U_SS, U_AO
  REAL(Pr) :: kT
  INTEGER  :: I

  !-- inputs --
  n_pot       = 36.0_Pr
  sigma_c     = 4.30_Pr
  sigma_p     = 1.4921_Pr
  phi_p       = 0.56_Pr
  alpha3      = 0.1_Pr
  alpha1      = 6.3806423321775344605E22_Pr
  kT          = 1.0_Pr

  !-- The number pi 
  pi = 4.0_Pr * atan(1.0_Pr)

  !-- The constants for the Asakura-Oosawa potential are computed 
  zeta = sigma_p / sigma_c
  alpha2 = (1.0_Pr + zeta) * sigma_c
 
  B = 3.0_Pr * phi_p / (4.0_Pr * alpha3 * zeta**4.0_Pr * sigma_c**2.0_Pr) * &
       ((1.0_Pr + zeta)**2.0_Pr - (1.0_Pr + alpha3 * zeta)**2.0_Pr)

  aux = sigma_c * (1.0_Pr + alpha3 * zeta)
  C = -phi_p * ((1.0_Pr + zeta)/zeta)**3.0_Pr * &
       (1.0_Pr - 3.0_Pr * aux / sigma_c / (2.0_Pr * (1.0_Pr + zeta)) + &
       0.5_Pr * (aux / sigma_c / (1.0_Pr + zeta))**3.0_Pr) - &
       B * (alpha3 * zeta * sigma_c)**2.0_Pr

  join_point = sigma_c * (1.0_Pr + alpha3 * zeta)

  
  !-- Soft repulsive potential --
  U_ss = alpha1 * kt * &
       (1.0_Pr/(x**(n_pot)) - 2.0_Pr/(alpha2**(n_pot/2.0_Pr) * &
          x**(n_pot/2.0_Pr)) + 1.0_Pr/alpha2**n_pot)
  
  !-- Asakura-Oosawa potential joined to a square part to ensure some
  !   restrictions. --
  IF (x .LE. join_point) THEN
     U_AO = kt * B * (x - sigma_c)**2.0_Pr + C
  ELSE
     U_AO = - kt * phi_p * &
          ((1.0_Pr + zeta)/zeta)**3.0_Pr * &
          (1.0_Pr - 3.0_Pr * x /sigma_c / (2.0_Pr * (1.0_Pr + zeta)) + &
          0.5_Pr * (x / sigma_c / (1.0_Pr + zeta))**3.0_Pr)
  ENDIF

  potential_AO_soft = U_ss + U_AO

END FUNCTION potential_AO_soft
