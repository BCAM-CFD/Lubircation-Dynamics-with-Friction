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
SUBROUTINE obtain_list_dirs(dir, list_dirs, error_out)
!----------------------------------------
  ! The list of directories from a given directory are obtained.
  !*** Be careful: list_files can be allocated at the end of the
  !    subroutine, even if it finishes with error. It should be deallocated
  !    in the main program. ***
  !--------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: dir
  CHARACTER(LEN=MAX_CHAR), DIMENSION(:), ALLOCATABLE, INTENT(out) :: list_dirs
  INTEGER, INTENT(out)    :: error_out
  INTEGER                 :: N_dirs
  CHARACTER(LEN=MAX_CHAR) :: temporal_file
  LOGICAL                 :: control_end_file
  INTEGER                 :: counter
  INTEGER                 :: ios
  INTEGER                 :: unit
  INTEGER                 :: I
    
  error_out = 0

  temporal_file = '.xjfd2345'

  !-- We search for a free unit --
  CALL search_unit(unit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Command to list only directories (not files) --
  CALL SYSTEM('ls -p '//trim(dir)//' | grep / > '//&
       trim(temporal_file))
  !-- Number of dirs is computed (= Number of lines) --
  CALL file_number_lines(temporal_file, N_dirs, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
  IF (N_dirs == 0) THEN
     WRITE(*,*)  '*** Obtain list files from_dir warning: there are 0 dirs in ',&
          trim(dir),' directory.'
     WRITE(*,*) '    or that directory does not exist. ***'
!     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF
  ALLOCATE(list_dirs(N_dirs))

  OPEN(unit, FILE=temporal_file, STATUS='OLD')
  control_end_file = .FALSE.
  counter = 0
  DO WHILE(.NOT.(control_end_file))
     READ(unit,'(A)',iostat = ios) list_dirs(counter + 1)
     IF (ios == 0) THEN !--- Si el archivo se pudo leer ---
        counter = counter + 1
     ELSE
        Control_end_file = .TRUE.
     ENDIF
  ENDDO
  CLOSE(unit)
  IF (N_dirs .NE. counter) THEN
     WRITE(*,*) '*** Obtain list dirs from dir error: Number of dirs is not equal' 
     WRITE(*,*) '    to the number of objects obtained by ls. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine
  ENDIF

1000 CONTINUE

  !-- The last slash in every dir is deleted --
  DO I = 1, N_dirs
     list_dirs(I) = list_dirs(I)(1:len_trim(list_dirs(I)) - 1)
  ENDDO

  !-- Temporal file is removed --
  CALL SYSTEM('rm '//trim(temporal_file))

END SUBROUTINE obtain_list_dirs
