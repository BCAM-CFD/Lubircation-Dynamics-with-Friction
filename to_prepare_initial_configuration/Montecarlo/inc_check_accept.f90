!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE check_accept(this, V_nm, accept)
  !-----------------------------------------------
  ! This subroutine decides if a movement is 
  ! or not accepted. See the Metropolis method
  ! in the Allen and Tildesley for more information.
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(montecarlo_type), INTENT(in) :: this
  REAL(Pr), INTENT(in)              :: V_nm !-- The change in the potential --
  LOGICAL, INTENT(out)              :: accept
  REAL(Pr) :: random

  IF (this%beta * V_nm < 75.0) THEN 
     IF (V_nm .LE. 0) THEN
        accept = .TRUE.
     ELSE
        CALL RANDOM_NUMBER(random)
        IF (random < exp(-this%beta * V_nm)) THEN
           accept = .TRUE.
        ELSE
           accept = .FALSE.
        ENDIF
     ENDIF
  ELSE
     !-- This is a computational issue, to avoid underflow in the 
     !   computation of the exponential. --
     accept = .FALSE.
  ENDIF

END SUBROUTINE check_accept
