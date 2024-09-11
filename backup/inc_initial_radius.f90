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
SUBROUTINE initial_radius(this, R)
  !------------------------------------------------
  ! The radius is assigned to every particle.
  !-----------------------------------------------
  use class_files_utilities
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER :: I
  INTEGER :: error_out
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: rad
  REAL(Pr), INTENT(in)             :: R
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --
  
  this%R_ave = 0.0_Pr
  
  error_out = 0
  
  file_name = 'inc_initial_radius.f90' 
  
  IF (this%read_rad) THEN
  
    CALL file_to_array(this%file_rad, rad,  error_out)
    
    IF (error_out .NE. 0) THEN
           CALL error_header(file_name) 
           WRITE(*,*) '*** initial radius error: file has not been read '
           write(*,*) '    properly ***'
           GOTO 1000 !-- End of subroutine --
    ENDIF
    

    IF (this%N .NE. SIZE(rad,1)) THEN
     CALL error_header(file_name)        
        WRITE(*,*) '*** initial radius error: the number of lines from'
        WRITE(*,*) '    the radius file does not match with the number of particles. ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
     
    DO I=1, this%N
    this%part(I)%R=rad(I,1)
    this%R_ave = this%R_ave + rad(I,1)
    END DO
    
    this%R_ave = this%R_ave / this%N
    write(*,*) this%R_ave
    
  ELSE
  
  this%part(:)%R = R
  this%R_ave = R
  
  END IF

1000 CONTINUE


  IF(ALLOCATED(rad)) THEN
  DEALLOCATE(rad)
  END IF
  
END SUBROUTINE initial_radius
