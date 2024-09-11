!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!*********************************************
!  CLASS FILES_UTILITIES FILE
!*********************************************

!--------------------------------------------------------------------
SUBROUTINE file_to_array_double(file, array, error_out, forced_unit)
!--------------------------------------------------------------------
  ! The columns of numbers from a file are taken to an array.
  ! Forced_unit can be used to force to the program to use a 
  ! predefined unit to deal with the file.
  ! array will be allocated, and will contain the data at the end
  ! of the subroutine.
  !------------------------------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in)                :: file
  REAL*8, DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: array
  INTEGER, INTENT(out)                               :: error_out
  INTEGER, INTENT(in), OPTIONAL                      :: forced_unit
  INTEGER :: N_columns
  INTEGER :: N_lines
  INTEGER :: unit
  INTEGER :: ios
  INTEGER :: I

  !-- An accessible unit is searched --
  IF (PRESENT(forced_unit)) THEN
     unit = forced_unit
  ELSE !-- If we do not care which unit to be used --
     CALL search_unit(unit, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDIF

  !-- Inquire order does not recognize the ~ symbol in some compilers -
  IF (file(1:1) == '~') THEN
     WRITE(*,*) '*** File to array error: Please write the complete &
          route without using the symbol ~ ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine
  ENDIF

  !-- Number of columns of the file are computed --
  CALL file_number_columns(file, N_columns, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Number of lines of the file are computed --
  CALL  file_number_lines(file, N_lines, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Array is allocated --
  IF (.NOT.(ALLOCATED(array))) THEN
     ALLOCATE(array(N_lines, N_columns))
  ELSE
     WRITE(*,*) '*** File to array error: array was already allocated. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- File is read --
  OPEN(unit, FILE = file, STATUS = 'OLD') 
  DO I = 1, N_lines
     READ(unit, *, iostat = ios) array(I,:)
     IF (ios .NE. 0) THEN !-- Some reading problem --
        WRITE(*,*) 'File to array error: reading problem with file ',&
             trim(file),' ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDDO
  CLOSE(unit)

1000 CONTINUE
  IF (error_out .NE. 0) THEN
     IF (ALLOCATED(array)) THEN
        DEALLOCATE(array)
     ENDIF
  ENDIF

END SUBROUTINE file_to_array_double

!--------------------------------------------------------------------
SUBROUTINE file_to_array_real(file, array, error_out)
!--------------------------------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in)                :: file
  REAL*4, DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: array
  INTEGER, INTENT(out)                               :: error_out
  INTEGER :: N_columns
  INTEGER :: N_lines
  INTEGER :: unit
  INTEGER :: ios
  INTEGER :: I

  !-- Number of columns of the file are computed --
  CALL file_number_columns(file, N_columns, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Number of lines of the file are computed --
  CALL  file_number_lines(file, N_lines, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Array is allocated --
  IF (.NOT.(ALLOCATED(array))) THEN
     ALLOCATE(array(N_lines, N_columns))
  ELSE
     WRITE(*,*) '*** File to array error: array was already allocated. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- File is read --
  OPEN(unit, FILE = file, STATUS = 'OLD') 
  DO I = 1, N_lines
     READ(unit, *, iostat = ios) array(I,:)
     IF (ios .NE. 0) THEN !-- Some reading problem --
        WRITE(*,*) 'File to array error: reading problem with file ',&
             trim(file),' ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDDO
  CLOSE(unit)

1000 CONTINUE

END SUBROUTINE file_to_array_real

!--------------------------------------------------------------------
SUBROUTINE file_to_array_integer(file, array, error_out)
!--------------------------------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in)                 :: file
  INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: array
  INTEGER, INTENT(out)                                :: error_out
  INTEGER :: N_columns
  INTEGER :: N_lines
  INTEGER :: unit
  INTEGER :: ios
  INTEGER :: I

  !-- Number of columns of the file are computed --
  CALL file_number_columns(file, N_columns, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Number of lines of the file are computed --
  CALL  file_number_lines(file, N_lines, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Array is allocated --
  IF (.NOT.(ALLOCATED(array))) THEN
     ALLOCATE(array(N_lines, N_columns))
  ELSE
     WRITE(*,*) '*** File to array error: array was already allocated. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- File is read --
  OPEN(unit, FILE = file, STATUS = 'OLD') 
  DO I = 1, N_lines
     READ(unit, *, iostat = ios) array(I,:)
     IF (ios .NE. 0) THEN !-- Some reading problem --
        WRITE(*,*) 'File to array error: reading problem with file ',&
             trim(file),' ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDDO
  CLOSE(unit)

1000 CONTINUE

END SUBROUTINE file_to_array_integer
  
