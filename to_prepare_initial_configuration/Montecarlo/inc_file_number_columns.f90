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

!-------------------- file_number_columns ------------------

!------------------------------------------------------
SUBROUTINE file_number_columns(file, Ncolumns, error_out)
!------------------------------------------------------
  ! Calculation of the number of columns of file
  !----------------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: file
  INTEGER, INTENT(out)                :: Ncolumns
  INTEGER, INTENT(out)                :: error_out  
  LOGICAL :: control
  INTEGER :: unit, ios
  REAL, DIMENSION(:), ALLOCATABLE :: data_row
  CHARACTER(LEN=MAX_CHAR)         :: line
  LOGICAL                         :: exists

  error_out = 0

  !-- We check if file exists --
  INQUIRE(FILE = file, EXIST = exists)
  IF (.NOT.(exists)) THEN
     WRITE(*,*) '*** File number columns error: File ', trim(file),&
          ' does not exist. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine
  ENDIF

  CALL search_unit(unit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  Ncolumns = 0
  control   = .TRUE.
  DO WHILE(control)
     OPEN(unit, FILE=trim(file), STATUS='OLD') 
     Ncolumns = Ncolumns + 1
     ALLOCATE(data_row(Ncolumns))
     READ(unit,'(A)') line
     READ(line, *, iostat = ios) data_row(:)
     IF (ios .NE. 0) THEN !--- If line could not be read ---
        Ncolumns = Ncolumns - 1
        control = .FALSE.
     ENDIF
     DEALLOCATE(data_row)
     CLOSE(unit)
  ENDDO

!  WRITE(*,*) '*** Number of columns = ', Ncolumns  

1000 CONTINUE

END SUBROUTINE file_number_columns

!---------------- END file_number_columns ------------------
