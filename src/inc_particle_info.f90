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
!-------------------------------------------
  SUBROUTINE particle_info(this)
!-------------------------------------------
    ! This subroutine writes in the shell information
    ! about the particle object.
    !---------------------------------------
    IMPLICIT NONE
    TYPE(particle_type), INTENT(in) :: this
    INTEGER :: I
    
    IF (ALLOCATED(this%pos)) THEN
       WRITE(*,*) 'Pos     = ', this%pos(:)
    ELSE
       WRITE(*,*) 'Pos not allocated'
    ENDIF

    IF (ALLOCATED(this%vel)) THEN
       WRITE(*,*) 'Vel     = ', this%vel(:)
    ELSE
       WRITE(*,*) 'Vel not allocated'
    ENDIF

    IF (ALLOCATED(this%Omega)) THEN
       WRITE(*,*) 'Omega     = ', this%Omega(:)
    ELSE
       WRITE(*,*) 'Omega not allocated'
    ENDIF

    IF (ALLOCATED(this%force)) THEN
       WRITE(*,*) 'Force   = ', this%force(:)
    ELSE
       WRITE(*,*) 'Force not allocated'
    ENDIF

    WRITE(*,*) 'R       = ', this%R
    WRITE(*,*) 'mass    = ', this%mass

    IF (ALLOCATED(this%pos0)) THEN
       WRITE(*,*) 'Pos0    = ', this%pos0(:)
    ELSE
       WRITE(*,*) 'Pos0 not allocated'
    ENDIF

    WRITE(*,*) 'N_neigh = ', this%N_neigh    

    WRITE(*,*) '---- List of neighbours ----'
    IF (this%N_neigh .NE. 0) THEN
       DO I = 1, this%N_neigh
          WRITE(*,*) this%neigh_list(I)
       ENDDO
    ENDIF

  END SUBROUTINE particle_info
