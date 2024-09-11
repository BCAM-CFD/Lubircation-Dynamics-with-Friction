!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!   Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Jose Antonio Ruiz Lopez, currently from the Department of applied physics at
!   Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!   in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------

!----------------------------------
SUBROUTINE particle_destructor(this)
!----------------------------------
  ! Destructor of the class particle
  !--------------------------------
  IMPLICIT NONE
  TYPE(particle_type), INTENT(inout) :: this
  
  IF (ALLOCATED(this%pos)) THEN
     DEALLOCATE(this%pos)
  ENDIF
  
  IF (ALLOCATED(this%vel)) THEN
     DEALLOCATE(this%vel)
  ENDIF

  IF (ALLOCATED(this%Omega)) THEN
     DEALLOCATE(this%Omega)
  ENDIF

  IF (ALLOCATED(this%force)) THEN
     DEALLOCATE(this%force)
  ENDIF
  
  IF (ALLOCATED(this%torque)) THEN
     DEALLOCATE(this%torque)
  ENDIF

  IF (ALLOCATED(this%acc)) THEN
     DEALLOCATE(this%acc)
  ENDIF

  IF (ALLOCATED(this%neigh_list)) THEN
     DEALLOCATE(this%neigh_list)
  ENDIF
  
  
  IF (ALLOCATED(this%cont_list)) THEN
     DEALLOCATE(this%cont_list)
  ENDIF

  IF (ALLOCATED(this%pos0)) THEN
     DEALLOCATE(this%pos0)
  ENDIF

  IF (ALLOCATED(this%Spp)) THEN
     DEALLOCATE(this%Spp)
  ENDIF

  IF (ALLOCATED(this%Spp_rep)) THEN
     DEALLOCATE(this%Spp_rep)
  ENDIF

  IF (ALLOCATED(this%Spp_con)) THEN
     DEALLOCATE(this%Spp_con)
  ENDIF
  
  
  IF (ALLOCATED(this%Spp_lub_norm)) THEN
     DEALLOCATE(this%Spp_lub_norm)
  ENDIF

  IF (ALLOCATED(this%Spp_lub_tang)) THEN
     DEALLOCATE(this%Spp_lub_tang)
  ENDIF
  
  IF (ALLOCATED(this%def_tan)) THEN
     DEALLOCATE(this%def_tan)
  ENDIF
  
  IF (ALLOCATED(this%def_topw)) THEN
     DEALLOCATE(this%def_topw)
  ENDIF
    
  IF (ALLOCATED(this%def_botw)) THEN
     DEALLOCATE(this%def_botw)
  ENDIF

END SUBROUTINE particle_destructor
