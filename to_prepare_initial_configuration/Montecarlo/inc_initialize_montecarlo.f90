!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------
SUBROUTINE initialize_montecarlo(this, rcut, kT, unit)
!------------------------------------------------------------
  ! The montecarlo object is initialized.
  !----------------------------------------------------------
  IMPLICIT NONE
  TYPE(montecarlo_type), INTENT(inout) :: this
  REAL(Pr), INTENT(in)                 :: rcut
  REAL(Pr), INTENT(in)                 :: kT
  INTEGER, INTENT(In)                  :: unit

  IF (this%amplitude .LE. 0) THEN
     this%amplitude = 0.5_Pr * rcut
  ENDIF

  this%beta = 1.0_Pr / kT

  WRITE(unit, *) '*** Montecarlo initialized ***'

END SUBROUTINE initialize_montecarlo
