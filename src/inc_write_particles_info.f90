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
!---------------------------------------
SUBROUTINE write_particles_info(this)
!---------------------------------------
  ! Subroutine to write in the shell information
  ! about all the particles.
  !-------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(in) :: this
  INTEGER :: I

  IF (this%N == 0 .OR. .NOT.(ALLOCATED(this%part))) THEN
     WRITE(*,*) '** Sorry, there are not particles to display information. **'
  ELSE
     DO I = 1, this%N
        WRITE(*,*) '****** Particle',I,'***********'
        CALL particle_info(this%part(I))
        WRITE(*,*) 
     ENDDO
  ENDIF

END SUBROUTINE write_particles_info
