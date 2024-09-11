!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------------------------------------------------
SUBROUTINE initialize_potential(this, rcut, unit)
!--------------------------------------------------------
  IMPLICIT NONE
  TYPE(potential_type), INTENT(inout) :: this
  REAL(Pr), INTENT(in)                :: rcut
  INTEGER, INTENT(In)                 :: unit

  this%rcut = rcut
  this%aux  = REAL(this%Ntable, KIND = Pr) / this%rcut
!  CALL build_table(this)

  WRITE(unit, *) '*** Potential initialized ***'

END SUBROUTINE initialize_potential
