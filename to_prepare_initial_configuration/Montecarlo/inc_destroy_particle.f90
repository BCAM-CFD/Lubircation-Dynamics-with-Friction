!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------
  SUBROUTINE destroy_particle(this)
!------------------------------------------------------
    ! Destructor of a particle 
    !--------------------------------------------------
    IMPLICIT NONE
    TYPE(particle_type), INTENT(inout) :: this
    
    IF (ALLOCATED(this%pos)) THEN
       DEALLOCATE(this%pos)
    ENDIF
    IF (ALLOCATED(this%old_pos)) THEN
       DEALLOCATE(this%old_pos)
    ENDIF
    
  END SUBROUTINE destroy_particle
