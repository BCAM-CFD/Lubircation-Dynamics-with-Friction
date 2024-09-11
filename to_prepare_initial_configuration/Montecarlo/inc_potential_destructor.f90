!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------------
SUBROUTINE potential_destructor(this)
!-------------------------------------------------------
  IMPLICIT NONE
  TYPE(potential_type), INTENT(inout) :: this
  
  IF (ALLOCATED(this%val)) THEN
     DEALLOCATE(this%val)
  ENDIF

END SUBROUTINE potential_destructor
