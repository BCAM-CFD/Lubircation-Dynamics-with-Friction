!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Sagaya Savarimuthu Prasanna Kumar, from Marco Ellero's group.
! - Jose Antonio Ruiz-Lopez, from Marco Ellero's group.
!    Currently from the Department of Applied Physics at
!    Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Sagaya Savarimuthu Prasanna Kumar.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------
!--------------------------------------------
SUBROUTINE destroy_system(this)
!--------------------------------------------
  ! To release memory, this subroutine destroys 
  ! all the objects used during the simulation.
  !------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER :: I, J, K

  IF (ALLOCATED(this%part)) THEN
     DO I = 1, SIZE(this%part)
        CALL particle_destructor(this%part(I))
     ENDDO
     DEALLOCATE(this%part)
  ENDIF

  IF (ALLOCATED(this%L)) THEN
     DEALLOCATE(this%L)
  ENDIF

  IF (ALLOCATED(this%Spp)) THEN
     DEALLOCATE(this%Spp)
  ENDIF
  
  IF (ALLOCATED(this%Spp_con)) THEN
     DEALLOCATE(this%Spp_con)
  ENDIF
  
  IF (ALLOCATED(this%Spp_rep)) THEN
     DEALLOCATE(this%Spp_rep)
  ENDIF
  
  IF (ALLOCATED(this%Spp_lub)) THEN
     DEALLOCATE(this%Spp_lub)
  ENDIF
  
  IF (ALLOCATED(this%force_plane_x)) THEN
     DEALLOCATE(this%force_plane_x)
  ENDIF

  IF (ALLOCATED(this%force_plane_y)) THEN
     DEALLOCATE(this%force_plane_y)
  ENDIF

  IF (ALLOCATED(this%force_plane_z)) THEN
     DEALLOCATE(this%force_plane_z)
  ENDIF    

  CALL wall_destructor(this%wall)

  CALL destroy_output(this%output)

  IF (ALLOCATED(this%cell)) THEN
     DO I = 1, SIZE(this%cell,1)
        DO J = 1, SIZE(this%cell,2)
           DO K = 1, SIZE(this%cell,3)
              CALL cell_destructor(this%cell(I,J,K))
           ENDDO
        ENDDO
     ENDDO
     DEALLOCATE(this%cell)
  ENDIF

  WRITE(*,*) '*** All memory released ***'

END SUBROUTINE destroy_system
