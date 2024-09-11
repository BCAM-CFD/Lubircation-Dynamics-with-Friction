!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------------
SUBROUTINE destroy_physics(this)
!-------------------------------------------------------
  IMPLICIT NONE
  TYPE(physics_type), INTENT(inout) :: this
  INTEGER :: I

  IF (ALLOCATED(this%Box)) THEN
     DEALLOCATE(this%Box)
  ENDIF

  IF (ALLOCATED(this%part)) THEN
     DO I = 1, SIZE(this%part)
        CALL destroy_particle(this%part(I))
     ENDDO
     DEALLOCATE(this%part)
  ENDIF

END SUBROUTINE destroy_physics
