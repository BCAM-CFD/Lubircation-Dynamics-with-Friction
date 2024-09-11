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

!------------------------------------------------
SUBROUTINE check_directory(dir, exists, error_out)
!------------------------------------------------
  ! Subroutine to check if a directory exists.
  !----------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(inout) :: dir
  LOGICAL, INTENT(out)                   :: exists
  INTEGER, INTENT(out)                   :: error_out
  INTEGER :: I
  INTEGER :: pos_slash
  CHARACTER(LEN=MAX_CHAR) :: base_dir
  CHARACTER(LEN=MAX_CHAR) :: dir_to_find
  CHARACTER(LEN=MAX_CHAR), DIMENSION(:), ALLOCATABLE ::list_dirs
  
  error_out = 0

  !-- We do some rearrangements. If last character is a slash,
  !   it is deleted. --
  IF (len_trim(dir) .NE. 1) THEN
     IF (dir(len_trim(dir):len_trim(dir)) == '/') THEN
        dir = dir(1:len_trim(dir) - 1)
     ENDIF
  ENDIF
  
  !-- First we find where the last slash is in the dir name --
  DO I = len_trim(dir), 1, -1
     IF (dir(I:I) == '/') THEN
        pos_slash = I
        GOTO 10 !-- Out of the loop --
     ENDIF
  ENDDO
  !-- If this line is reached, there is not a slash --
  pos_slash = 0
10 CONTINUE

  IF (pos_slash > 1) THEN
     base_dir = dir(1:pos_slash -1)
  ELSE IF (pos_slash == 1) THEN
     base_dir = '/'
     IF (trim(dir) == '/') THEN
        WRITE(*,*) '** Class files utilities error: the program is '
        WRITE(*,*) '   not able to determine if / exists, but it should, isn''t it?'
        error_out = 1 !-- End of subroutine --
        GOTO 1000
     ENDIF
  ELSE !-- pos_slash = 0 --
     IF (trim(dir) == '~') THEN
        WRITE(*,*) '** Class files utilities error: the program is '
        WRITE(*,*) '   not able to determine if ~ exists, but it should, isn''t it?'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ELSE
        base_dir = '.'
     ENDIF
  ENDIF

  !-- The list of directories from base_dir is constructed --
  CALL obtain_list_dirs(base_dir, list_dirs, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Now we check if the directory is in the list --
  dir_to_find = dir(pos_slash + 1: len_trim(dir))
  exists = .FALSE.
  DO I = 1, SIZE(list_dirs)
     IF (dir_to_find == list_dirs(I)) THEN
        exists = .TRUE.
        GOTO 100 !-- End of loop --
     ENDIF
  ENDDO
100 CONTINUE  
    
1000 CONTINUE
  IF (ALLOCATED(list_dirs)) THEN
     DEALLOCATE(list_dirs)
  ENDIF


END SUBROUTINE check_directory
