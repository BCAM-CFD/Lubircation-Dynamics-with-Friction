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
!--------------------------------------------------------
SUBROUTINE random_constructor(this, unit)
!--------------------------------------------------------
  ! Constructor de la clase random
  !------------------------------------------------------
  IMPLICIT NONE
  TYPE(random_type), INTENT(inout) :: this
  INTEGER, INTENT(in)              :: unit
  INTEGER, DIMENSION(8) :: time_array

  IF (this%fixed_seed) THEN
     WRITE(unit,*) '*** Seed is fixed ***'
  ENDIF

  CALL init_random_seed(this%fixed_seed, this%seed)
  WRITE(unit,*) '*** Random number generator initialized ***'
  WRITE(unit,*) '*** seed = ',this%seed,' ***'

  this%iset = 0

END SUBROUTINE random_constructor
