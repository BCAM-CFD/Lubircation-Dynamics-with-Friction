!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------
SUBROUTINE build_table(this)
!-------------------------------------------------------------
  IMPLICIT NONE
  TYPE(potential_type), INTENT(inout) :: this
  INTEGER  :: I
  REAL(Pr) :: x

  ALLOCATE(this%val(this%Ntable))

  DO I = 1, this%Ntable
     ! The value of the argument of the I piece is I/Ntable
     ! x represents r/this%rcut
     x = REAL(I, KIND = Pr) / this%aux

     this%val(I) = potential(x )

  ENDDO

END SUBROUTINE build_table
