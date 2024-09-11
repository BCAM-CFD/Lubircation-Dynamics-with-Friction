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

!-------------------- file_number_lines ------------------

!------------------------------------------------------
SUBROUTINE file_number_lines(file, Nlines, error_out)
!------------------------------------------------------
  ! Calculation of the number of lines of file
  !----------------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: file
  INTEGER, INTENT(out)                :: Nlines
  INTEGER, INTENT(out)                :: error_out  
  LOGICAL                         :: control
  INTEGER                         :: unit, ios
  REAL, DIMENSION(:), ALLOCATABLE :: data_row
  CHARACTER(LEN=MAX_CHAR)         :: line
  LOGICAL                         :: exists
  CHARACTER(LEN=MAX_CHAR)         :: temporal_file

  error_out = 0
  
  temporal_file = '.temp564423kfd'
  
  !-- We check if file exists --
  INQUIRE(FILE = file, EXIST = exists)
  IF (.NOT.(exists)) THEN
     WRITE(*,*) '*** File number lines error: File ', trim(file),&
          'does not exist. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine
  ENDIF

  !-- We search for a free unit --
  CALL search_unit(unit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  CALL SYSTEM('wc -l < '//trim(file)//' > '//trim(temporal_file))
  OPEN(unit, FILE=temporal_file, STATUS='OLD') 
  READ(unit,'(I30)') Nlines
  CLOSE(unit)

1000 CONTINUE

  CALL SYSTEM('rm '//trim(temporal_file))

END SUBROUTINE file_number_lines

!---------------- END file_number_lines ------------------
