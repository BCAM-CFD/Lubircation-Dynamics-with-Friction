!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------------------------------------------------
  SUBROUTINE cell_destructor(this)
!--------------------------------------------------------
    ! Destructor of the class cell.
    !----------------------------------------------------
    IMPLICIT NONE
    TYPE(cell_type), INTENT(inout) :: this
    INTEGER :: I

    IF (ALLOCATED(this%L)) THEN
       DEALLOCATE(this%L)
    ENDIF
    IF (ALLOCATED(this%neigh)) THEN
       DO I = 1, SIZE(this%neigh)
          CALL coord_destructor(this%neigh(I))
       ENDDO
       DEALLOCATE(this%neigh)
    ENDIF
    IF (ALLOCATED(this%list_part)) THEN
       DEALLOCATE(this%list_part)
    ENDIF
    
  END SUBROUTINE cell_destructor
  
