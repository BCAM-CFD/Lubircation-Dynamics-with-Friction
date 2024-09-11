!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------
FUNCTION potential_table(this, x)
!------------------------------------------------------------
  ! Function that gives the potential energy for a given
  ! distance x, from a table build previously.
  !----------------------------------------------------------
  IMPLICIT NONE
  REAL(Pr)                         :: potential_table
  TYPE(potential_type), INTENT(in) :: this
  REAL(Pr), INTENT(in)             :: x
  INTEGER :: I
  
  I = INT(x * this%aux)
  
  potential_table = this%val(I)

END FUNCTION potential_table
