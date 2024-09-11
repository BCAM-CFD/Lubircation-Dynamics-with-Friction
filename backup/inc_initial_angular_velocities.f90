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
SUBROUTINE initial_ang_velocities(this, error_out)
  !------------------------------------------------
  ! The angular velocities are assigned to the particles
  !------------------------------------------------
  use class_files_utilities
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Omega
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --

  error_out = 0

  file_name = 'inc_initial_angular_velocities.f90'  

  IF (this%read_Omega) THEN
     !-------- Particles are read from a file ----------
     CALL file_to_array(this%file_Omega, Omega, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF
     IF (this%dim .NE. SIZE(Omega,2)) THEN
     CALL error_header(file_name)        
        WRITE(*,*) '*** initial angular velocities error: the number of columns from'
        WRITE(*,*) '    the angular velocities file does not match with the number of dimensions. ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
     this%N = SIZE(Omega,1)
     IF (this%N <= 0) THEN
        CALL error_header(file_name)                
        WRITE(*,*) '*** initial angular velocities error: Wrong number of particles ***'
        WRITE(*,*) 'N = ',this%N
        GOTO 1000 !-- End of subroutine --
     ENDIF
     DO I = 1, SIZE(this%part)
        this%part(I)%Omega(:) = Omega(I,:)
     ENDDO
  ELSE !-- angular velocities are not read from a file --
     DO I = 1, SIZE(this%part)
        this%part(I)%Omega(:) = 0.0_Pr
     ENDDO
  ENDIF

1000 CONTINUE

  IF (ALLOCATED(Omega)) THEN
     DEALLOCATE(Omega)
  ENDIF

END SUBROUTINE initial_ang_velocities
