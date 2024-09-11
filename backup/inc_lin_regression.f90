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

!*********************************************
!  CLASS FUNCTIONS_UTILITIES SUBROUTINE
!*********************************************

!----------------------------------------------
SUBROUTINE lin_regression(this, alpha, beta, r, error_out)
!----------------------------------------------
  ! The linear regression y = alpha + beta * x
  ! of an array (component 1 is x, component 2 is y)
  ! is obtained. Also its sample correlation coefficient.
  ! See http://en.wikipedia.org/wiki/Simple_linear_regression
  ! for more information.
  !-------------------------------------------
  IMPLICIT NONE
  REAL(Pr), DIMENSION(:,:), INTENT(in) :: this
  REAL(Pr), INTENT(out) :: alpha
  REAL(Pr), INTENT(out) :: beta
  REAL(Pr), INTENT(out) :: r
  INTEGER, INTENT(out)  :: error_out
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: xy
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: x_sq  
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: y_sq  
  REAL(Pr) :: mean_val_x
  REAL(Pr) :: mean_val_y
  REAL(Pr) :: mean_val_xy
  REAL(Pr) :: mean_val_x_sq
  REAL(Pr) :: mean_val_y_sq

  error_out = 0
  IF (SIZE(this(1,:)) .NE. 2) THEN
     error_out = 1
     WRITE(*,*) '*** lin_regression error: the second component of this(:,:) '
     WRITE(*,*) '    should be of size 2. ***'
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Some arrays are calculated --
  ALLOCATE(xy(SIZE(this(:,1))))
  ALLOCATE(x_sq(SIZE(this(:,1))))
  ALLOCATE(y_sq(SIZE(this(:,1))))
  xy(:)   = this(:,1) * this(:,2)
  x_sq(:) = this(:,1) * this(:,1)
  y_sq(:) = this(:,2) * this(:,2)

  !-- Several mean values are calculated --
  CALL mean_val(this(:,1), mean_val_x)
  CALL mean_val(this(:,2), mean_val_y)
  CALL mean_val(xy, mean_val_xy)
  CALL mean_val(x_sq, mean_val_x_sq)
  CALL mean_val(y_sq, mean_val_y_sq)

  !-- alpha, beta and r are calculated --
  beta = (mean_val_xy - mean_val_x * mean_val_y) / &
       (mean_val_x_sq - mean_val_x**2.0_Pr)
  
  alpha = mean_val_y - beta * mean_val_x

  r = (mean_val_xy - mean_val_x * mean_val_y) / &
       SQRT((mean_val_x_sq - mean_val_x**2.0_Pr) * &
       (mean_val_y_sq - mean_val_y**2.0_Pr))

1000 CONTINUE
  
  IF (ALLOCATED(xy)) THEN
     DEALLOCATE(xy)
  ENDIF
  IF (ALLOCATED(x_sq)) THEN
     DEALLOCATE(x_sq)
  ENDIF
  IF (ALLOCATED(y_sq)) THEN
     DEALLOCATE(y_sq)
  ENDIF

END SUBROUTINE lin_regression
