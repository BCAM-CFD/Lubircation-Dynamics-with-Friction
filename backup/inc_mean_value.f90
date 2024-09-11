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
!  CLASS FUNCTIONS_UTILITIES SUBROUTINE
!*********************************************

!------------------------------------------------
SUBROUTINE mean_val(this, mean)
!------------------------------------------------
  ! The mean value of an array is obtained.
  !----------------------------------------------
  IMPLICIT NONE
  REAL(Pr), DIMENSION(:), INTENT(in) :: this
  REAL(Pr), INTENT(out)              :: mean
  INTEGER :: I

  mean = 0.0_Pr
  DO I = 1, SIZE(this)
     mean = mean + this(I)
  ENDDO
  mean = mean / REAL(SIZE(this), KIND = Pr)

END SUBROUTINE mean_val
