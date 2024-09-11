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

!------------------------------------------------
SUBROUTINE initial_velocities(this, error_out)
  !------------------------------------------------
  ! The velocities are assigned to the particles
  !------------------------------------------------
  use class_files_utilities
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: vel
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --

  error_out = 0

  file_name = 'inc_initial_velocities.f90'  

  IF (this%read_vel) THEN
     !-------- Particles are read from a file ----------
     CALL file_to_array(this%file_vel, vel, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF
     IF (this%dim .NE. SIZE(vel,2)) THEN
     CALL error_header(file_name)        
        WRITE(*,*) '*** initial velocities error: the number of columns from'
        WRITE(*,*) '    the velocities file does not match with the number of dimensions. ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
     this%N = SIZE(vel,1)
     IF (this%N <= 0) THEN
        CALL error_header(file_name)                
        WRITE(*,*) '*** initial velocities error: Wrong number of particles ***'
        WRITE(*,*) 'N = ',this%N
        GOTO 1000 !-- End of subroutine --
     ENDIF
     DO I = 1, SIZE(this%part)
        this%part(I)%vel(:) = vel(I,:)
     ENDDO
  ELSE !-- Velocities are not read from a file --
     DO I = 1, SIZE(this%part)
        this%part(I)%vel(:) = 0.0_Pr
     ENDDO
  ENDIF

1000 CONTINUE

  IF (ALLOCATED(vel)) THEN
     DEALLOCATE(vel)
  ENDIF

END SUBROUTINE initial_velocities
