!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------------------------------------------------
  SUBROUTINE cell_system_destructor(this)
!--------------------------------------------------------
    ! Destructor of the class cell_system.
    !----------------------------------------------------
    IMPLICIT NONE
    TYPE(cell_system_type), INTENT(inout) :: this
    INTEGER :: I, J, K

    IF (ALLOCATED(this%coord)) THEN
       DO I = 1, SIZE(this%coord(:,1,1))
          DO J = 1, SIZE(this%coord(1,:,1))
             DO K = 1, SIZE(this%coord(1,1,:))
                CALL cell_destructor(this%coord(I,J,K))
             ENDDO
          ENDDO
       ENDDO
       DEALLOCATE(this%coord)
    ENDIF
    IF (ALLOCATED(this%Ncells)) THEN
       DEALLOCATE(this%Ncells)
    ENDIF

    WRITE(*,*) '*** cell_system object finished ***'

  END SUBROUTINE cell_system_destructor
