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

!----------------------------------------
SUBROUTINE obtain_list_files(dir, list_files, error_out)
!----------------------------------------
  ! The list of files of a given directory is obtained-
  ! No directories are saved.
  !*** Be careful: list_files can be allocated at the end of the
  !    subroutine, even if it finishes with error. It should be deallocated
  !    in the main program. ***
  !--------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in)                             :: dir
  CHARACTER(LEN=MAX_CHAR), DIMENSION(:), ALLOCATABLE, INTENT(out) :: list_files
  INTEGER, INTENT(out)    :: error_out
  INTEGER                 :: N_files
  CHARACTER(LEN=MAX_CHAR) :: temporal_file
  LOGICAL                 :: control_end_file
  INTEGER                 :: counter
  INTEGER                 :: ios
  INTEGER                 :: unit
  
  error_out = 0

  temporal_file = '.xjfd2345'

  !-- We search for a free unit --
  CALL search_unit(unit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Command to list only files (not directories) --
  CALL SYSTEM('ls -p '//trim(dir)//' | grep -v / > '//&
       trim(temporal_file))
  !-- Number of files is computed (= Number of lines) --
  CALL file_number_lines(temporal_file, N_files, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
  IF (N_files == 0) THEN
     WRITE(*,*)  '*** Obtain list files from_dir warning: there are 0 files in ',&
          trim(dir),' directory.'
     WRITE(*,*) '    or that directory does not exist. ***'
!     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF
  ALLOCATE(list_files(N_files))

  OPEN(unit, FILE=temporal_file, STATUS='OLD')
  control_end_file = .FALSE.
  counter = 0
  DO WHILE(.NOT.(control_end_file))
     READ(unit,'(A)',iostat = ios) list_files(counter + 1)
     IF (ios == 0) THEN !--- Si el archivo se pudo leer ---
        counter = counter + 1
     ELSE
        Control_end_file = .TRUE.
     ENDIF
  ENDDO
  CLOSE(unit)
  IF (N_files .NE. counter) THEN
     WRITE(*,*) '*** Obtain list files from dir error: Number of files is not equal' 
     WRITE(*,*) '    to the number of objects obtained by ls. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine
  ENDIF

1000 CONTINUE

  !-- Temporal file is removed --
  CALL SYSTEM('rm '//trim(temporal_file))

END SUBROUTINE obtain_list_files
