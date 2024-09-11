!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------------------------------------------------
  SUBROUTINE coord_destructor(this)
!--------------------------------------------------------
    ! Destructor of the class coord.
    !----------------------------------------------------
    IMPLICIT NONE
    TYPE(coord_type), INTENT(inout) :: this
    
    IF (ALLOCATED(this%coord)) THEN
       DEALLOCATE(this%coord)
    ENDIF
    
  END SUBROUTINE coord_destructor
