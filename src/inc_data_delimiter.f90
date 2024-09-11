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
!*********************************************
!  CLASS FUNCTIONS_UTILITIES SUBROUTINE
!*********************************************

!----------------------------------------------
SUBROUTINE data_delimiter(this, data_new, component, val_min, val_max, error_out)
  !----------------------------------------------
  ! The data from this, between val_min and val_max 
  ! in the component selected is stored in data_new
  !--------------------------------------------
  IMPLICIT NONE
  REAL(Pr), DIMENSION(:,:), INTENT(in) :: this
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: data_new
  INTEGER, INTENT(in)                  :: component
  REAL(Pr), INTENT(in)                 :: val_min
  REAL(Pr), INTENT(in)                :: val_max
  INTEGER, INTENT(out)                 :: error_out
  INTEGER :: counter
  INTEGER :: I

  error_out = 0

  IF (SIZE(this(1,:)) < component) THEN
     error_out = 1
     WRITE(*,*) '*** data delimiter error: the selected component s to high for the data array. ***'
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (component <= 0) THEN
     error_out = 1
     WRITE(*,*) '*** data_delimiter error: component should be a positive value. ***'
     GOTO 1000 !-- End of subroutine --
  ENDIF
  
  !-- First we check how many elements are going to be stored --
  counter = 0
  DO I = 1, SIZE(this(:,1))
     IF ((this(I,component) >= val_min) .AND. (this(I,component) <= val_max)) THEN
        counter = counter + 1
     ENDIF
  ENDDO
  
  !-- data_new is built --
  ALLOCATE(data_new(counter, SIZE(this(1,:))))
  counter = 0
  DO I = 1, SIZE(this(:,1))
     IF ((this(I,component) >= val_min) .AND. (this(I,component) <= val_max)) THEN
        counter = counter + 1
        data_new(counter, :) = this(I,:)
     ENDIF
  ENDDO

1000 CONTINUE

END SUBROUTINE data_delimiter
