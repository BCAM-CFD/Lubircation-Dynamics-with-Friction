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

!--------------------- read_variable -------------------------
! Here all the job is done in order to read a variable
!-------------------------------------------------------------

!--------------------------------------------------------
  SUBROUTINE read_variable_real(input_file, variable_name, &
       variable, error_out, checking)
!--------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(LEN=MAX_CHAR), INTENT(in) :: input_file
    CHARACTER(LEN=MAX_CHAR), INTENT(in) :: variable_name
    REAL*4, INTENT(inout)               :: variable
    INTEGER, INTENT(out)                :: error_out
    INTEGER, INTENT(in), OPTIONAL       :: checking
    TYPE(read_input_type) :: input
    error_out = 0
    
    CALL read_input_constructor(input, input_file, variable_name)
    CALL find_string(input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    CALL obtain_variable_string(input)
    IF (present(checking)) THEN
       CALL get_variable(input, variable, error_out, checking)
    ELSE
       CALL get_variable(input, variable, error_out)
    ENDIF
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

1000 CONTINUE

  END SUBROUTINE read_variable_real


!--------------------------------------------------------
  SUBROUTINE read_variable_double(input_file, variable_name, &
       variable, error_out, checking)
!--------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(LEN=MAX_CHAR), INTENT(in) :: input_file
    CHARACTER(LEN=MAX_CHAR), INTENT(in) :: variable_name
    REAL*8, INTENT(inout) :: variable
    INTEGER, INTENT(out) :: error_out
    INTEGER, INTENT(in), OPTIONAL       :: checking
    TYPE(read_input_type) :: input
    error_out = 0
    
    CALL read_input_constructor(input, input_file, variable_name)
    CALL find_string(input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    CALL obtain_variable_string(input)
    IF (present(checking)) THEN
       CALL get_variable(input, variable, error_out, checking)
    ELSE
       CALL get_variable(input, variable, error_out)
    ENDIF
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

1000 CONTINUE

  END SUBROUTINE read_variable_double

!--------------------------------------------------------
  SUBROUTINE read_variable_integer(input_file, variable_name, &
       variable, error_out, checking)
!--------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(LEN=MAX_CHAR), INTENT(in) :: input_file
    CHARACTER(LEN=MAX_CHAR), INTENT(in) :: variable_name
    INTEGER, INTENT(inout) :: variable
    INTEGER, INTENT(out) :: error_out
    INTEGER, INTENT(in), OPTIONAL       :: checking
    TYPE(read_input_type) :: input
    error_out = 0
    
    CALL read_input_constructor(input, input_file, variable_name)
    CALL find_string(input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    CALL obtain_variable_string(input)
    IF (present(checking)) THEN
       CALL get_variable(input, variable, error_out, checking)
    ELSE
       CALL get_variable(input, variable, error_out)
    ENDIF

    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

1000 CONTINUE

  END SUBROUTINE read_variable_integer

!--------------------------------------------------------
  SUBROUTINE read_variable_string(input_file, variable_name, &
       variable, error_out)
!--------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: input_file
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: variable_name
    CHARACTER(LEN=MAX_CHAR), INTENT(inout) :: variable
    INTEGER, INTENT(out)                   :: error_out
    TYPE(read_input_type) :: input
    error_out = 0
    
    CALL read_input_constructor(input, input_file, variable_name)
    CALL find_string(input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    CALL obtain_variable_string(input)
    CALL get_variable(input, variable)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

1000 CONTINUE

  END SUBROUTINE read_variable_string

!--------------------------------------------------------
  SUBROUTINE read_variable_logical(input_file, variable_name, &
       variable, error_out)
!--------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: input_file
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: variable_name
    LOGICAL, INTENT(inout)                 :: variable
    INTEGER, INTENT(out)                   :: error_out
    TYPE(read_input_type) :: input
    error_out = 0
    
    CALL read_input_constructor(input, input_file, variable_name)
    CALL find_string(input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    CALL obtain_variable_string(input)
    CALL get_variable(input, variable, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

1000 CONTINUE

  END SUBROUTINE read_variable_logical

!--------------------------------------------------------
  SUBROUTINE read_variable_array_integer(input_file, variable_name, &
       variable, error_out)
!--------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: input_file
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: variable_name
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(inout) :: variable
    INTEGER, INTENT(out)                   :: error_out
    TYPE(read_input_type) :: input
    error_out = 0
    
    CALL read_input_constructor(input, input_file, variable_name)
    CALL find_string(input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    CALL obtain_variable_string(input)
    CALL get_variable(input, variable, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

1000 CONTINUE

  END SUBROUTINE read_variable_array_integer

!--------------------------------------------------------
  SUBROUTINE read_variable_array_real(input_file, variable_name, &
       variable, error_out)
!--------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: input_file
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: variable_name
    REAL*4, DIMENSION(:), ALLOCATABLE, INTENT(inout) :: variable
    INTEGER, INTENT(out)                   :: error_out
    TYPE(read_input_type) :: input
    error_out = 0

    CALL read_input_constructor(input, input_file, variable_name)
    CALL find_string(input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    CALL obtain_variable_string(input)
    CALL get_variable(input, variable, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

1000 CONTINUE

  END SUBROUTINE read_variable_array_real

!--------------------------------------------------------
  SUBROUTINE read_variable_array_double(input_file, variable_name, &
       variable, error_out)
!--------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: input_file
    CHARACTER(LEN=MAX_CHAR), INTENT(in)    :: variable_name
    REAL*8, DIMENSION(:), ALLOCATABLE, INTENT(inout) :: variable
    INTEGER, INTENT(out)                   :: error_out
    TYPE(read_input_type) :: input

    error_out = 0
    
    CALL read_input_constructor(input, input_file, variable_name)
    CALL find_string(input, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    CALL obtain_variable_string(input)
    CALL get_variable(input, variable, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF

1000 CONTINUE

  END SUBROUTINE read_variable_array_double


!----------------- END read_variable -------------------------
