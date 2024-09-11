!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!*********************************************
!  CLASS READ_INPUT FILE
!*********************************************

!-------------------- find_string -------------------------

!--------------------------------------------------------------
  SUBROUTINE find_string(this, error_out)
!--------------------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout) :: this
    INTEGER, INTENT(out)                 :: error_out
    INTEGER :: unit
    LOGICAL :: control_end, found, twice, file_exists
    CHARACTER(LEN=MAX_CHAR) :: line_aux
    INTEGER :: length
    INTEGER :: ios
    
    error_out = 0

    CALL search_unit_b(unit, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

    !-- Firs we check that the input file exists --
    inquire(FILE = this%file, EXIST = file_exists)
    IF (.NOT.(file_exists)) THEN
       WRITE(*,*) '*** Input error: the file ', trim(this%file), ' does not exist. ***'
       error_out = 3
       GOTO 1000 !-- End of subroutine
    ENDIF

    length = len_trim(this%variable_name)
    this%length_name_variable = length
    
    OPEN(unit, FILE=this%file, STATUS='OLD')

    control_end = .FALSE.
    found  = .FALSE.
    twice       = .FALSE.
    DO WHILE(.NOT.(control_end))
       READ(unit, '(A1000)', iostat = ios) line_aux
       IF (ios == 0) THEN !-- If it could be read --
          line_aux = adjustl(line_aux)
          IF (line_aux(1:length) == this%variable_name) THEN
             IF (line_aux(length + 1:length + 1) == ' ') THEN
                IF (.NOT.(found)) THEN
                   this%line       = line_aux
                   found = .TRUE.
                ELSE
                   twice       = .TRUE.
                   control_end = .TRUE.
                ENDIF
             ENDIF
          ENDIF
       ELSE
          control_end = .TRUE.
       ENDIF
       
    ENDDO

    IF (twice) THEN
       WRITE(*,*) 
       WRITE(*,*) '*** Input error: variable ',trim(this%variable_name),&
            ' is repeated in input file. ***'
       WRITE(*,*) 
       error_out = 1
       GOTO 1000 !-- End of subroutine --
    ENDIF

    IF (.NOT.(found)) THEN
       WRITE(*,*) 
       WRITE(*,*) '*** Input error: variable ',trim(this%variable_name),&
            ' was not found in input file. ***'
       WRITE(*,*) 
       error_out = 2
       GOTO 1000 !-- End of subroutine --
    ENDIF
       
    
1000 CONTINUE

    CLOSE(unit)

  END SUBROUTINE find_string

!---------------- END find_string -------------------------
