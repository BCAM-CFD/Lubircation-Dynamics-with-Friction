!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------------------
SUBROUTINE destroy_sim(this)
!-------------------------------------------------------------
  ! The simulation object is destroyed 
  !-----------------------------------------------------------
  IMPLICIT NONE
  TYPE(sim_type), INTENT(inout) :: this

  IF (ALLOCATED(this%file)) THEN
     DEALLOCATE(this%file)
  ENDIF

  IF (ALLOCATED(this%forbidden)) THEN
     DEALLOCATE(this%forbidden)
  ENDIF

  CALL destroy_physics(this%physics)
  
  CALL cell_system_destructor(this%cells)
  
  CALL potential_destructor(this%potential)

END SUBROUTINE destroy_sim
