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
SUBROUTINE bit_function(this, Nbits, data_bits, valid_bits, error_out)
!----------------------------------------------
  ! If we have a lot of points for a given function
  ! with some noise we could want obtain an average of this
  ! data, in order to get something more similar to
  ! a function. This subroutine divides the space
  ! between the maximum and minimum value in Nbits, and obtain
  ! the mean value of the data in every bit.
  !---------------------------------------------
  IMPLICIT NONE
  REAL(Pr), DIMENSION(:,:), INTENT(in) :: this
  INTEGER, INTENT(in)                  :: Nbits
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE, INTENT(out) :: data_bits
  LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(out)    :: valid_bits
  INTEGER, INTENT(out)                 :: error_out
  REAL(Pr) :: xmin
  REAL(Pr) :: xmax
  REAL(Pr) :: dx
  REAL(Pr) :: dx_half
  INTEGER :: I, J
  INTEGER, DIMENSION(:), ALLOCATABLE :: counter

  IF (SIZE(this(1,:)) .NE. 2) THEN
     error_out = 1
     WRITE(*,*) '*** bit_function error: the second component of this(:,:) '
     WRITE(*,*) '    should be of size 2. ***'
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The length of a bit is calculated --
  xmin = MINVAL(this(:,1))
  xmax = MAXVAL(this(:,1))
  dx = (xmax - xmin) / REAL(Nbits, KIND = Pr)
  dx_half = 0.5_Pr * dx

  !-- The positions of the bits are calculated --
  ALLOCATE(data_bits(Nbits,2))
  ALLOCATE(counter(Nbits))
  ALLOCATE(valid_bits(Nbits))
  DO I = 1, Nbits
     data_bits(I,1) = xmin + REAL((I - 1), KIND = Pr) * dx + dx_half
  ENDDO

  !-- The values of the bits are calculated --
  data_bits(:,2) = 0.0_Pr
  counter(:)     = 0.0_Pr
  DO I = 1, SIZE(this(:,1))
     DO J = 1, Nbits
        IF ((this(I,1) >= data_bits(J,1) - dx_half) .AND. &
             (this(I,1) <= data_bits(J,1) + dx_half)) THEN
           data_bits(J,2) = data_bits(J,2) + this(I,2)
           counter(J) = counter(J) + 1
        ENDIF
     ENDDO
  ENDDO
  
  valid_bits(:) = .TRUE.
  DO I = 1, Nbits
     IF (counter(I) .NE. 0) THEN
        data_bits(I,2) = data_bits(I,2) / REAL(counter(I), KIND = Pr)
     ELSE
        valid_bits(I) = .FALSE.
     ENDIF
  ENDDO

1000 CONTINUE

  IF (ALLOCATED(counter)) THEN
     DEALLOCATE(counter)
  ENDIF

END SUBROUTINE bit_function
