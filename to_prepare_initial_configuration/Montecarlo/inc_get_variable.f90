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

!---------------------- get_variable ------------------------------
! In here, the value of the variable is taken from this%variable_string to 
! variable.
!---------------------------------------------------------------------

!---------------------------------------------------
  SUBROUTINE get_variable_double(this, variable, error_out, checking)
!---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout) :: this
    REAL*8, INTENT(out)                  :: variable
    INTEGER, INTENT(out)                 :: error_out
    INTEGER, INTENT(in), OPTIONAL        :: checking

    CALL get_format(this,2)
    READ(this%variable_string, this%format, &
         iostat = error_out) variable

    !-- Some checkings are made --
    IF (error_out == 0) THEN !-- If it could be read --
       IF (present(checking)) THEN
          include 'inc_number_checkings.f90'
       ENDIF
    ELSE
       error_out = 1
       WRITE(*,*) '*** get variable error ***'
       WRITE(*,*) '*** Possible error: this error happens when there '
       WRITE(*,*) '    is a tabulated space (invisible) behind the '
       WRITE(*,*) '    variables. Please, check that. ***'
    ENDIF

  END SUBROUTINE get_variable_double

!---------------------------------------------------
  SUBROUTINE get_variable_real(this, variable, error_out, checking)
!---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout) :: this
    REAL*4, INTENT(out)                  :: variable
    INTEGER, INTENT(out)                 :: error_out
    INTEGER, INTENT(in), OPTIONAL        :: checking

    CALL get_format(this,1)
    READ(this%variable_string, this%format, &
         iostat = error_out) variable

    !-- Some checkings are made --
    IF (error_out == 0) THEN !-- If it could be read --
       IF (present(checking)) THEN
          include 'inc_number_checkings.f90'
       ENDIF
    ELSE
       error_out = 1
       WRITE(*,*) '*** get variable error ***'
       WRITE(*,*) '*** Possible error: this error happens when there '
       WRITE(*,*) '    is a tabulated space (invisible) behind the '
       WRITE(*,*) '    variables. Please, check that. ***'
    ENDIF

  END SUBROUTINE get_variable_real

!---------------------------------------------------
  SUBROUTINE get_variable_integer(this, variable, error_out, checking)
!---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout) :: this
    INTEGER, INTENT(out)                 :: variable
    INTEGER, INTENT(out)                 :: error_out    
    INTEGER, INTENT(in), OPTIONAL        :: checking
    INTEGER :: error_int

    CALL get_format(this,3,error_int)
    IF (error_int == 2) THEN
       WRITE(*,*) 
       WRITE(*,*) '*** Input error: variable '//&
            trim(this%variable_name)//' is real and it should be &
            integer. Please, correct input file. ***'
       WRITE(*,*)
       error_out = 2
       GOTO 1000 !-- end of subroutine --
    ENDIF
    READ(this%variable_string, this%format, &
         iostat = error_out) variable

    !-- Some checkings are made --
    IF (error_out == 0) THEN !-- If it could be read --
       IF (present(checking)) THEN
          include 'inc_number_checkings.f90'
       ENDIF
    ELSE
       error_out = 1
       WRITE(*,*) '*** get variable error ***'
       WRITE(*,*) '*** Possible error: this error happens when there '
       WRITE(*,*) '    is a tabulated space (invisible) behind the '
       WRITE(*,*) '    variables. Please, check that. ***'
    ENDIF

1000 CONTINUE

  END SUBROUTINE get_variable_integer

!---------------------------------------------------
  SUBROUTINE get_variable_string(this, variable)
!---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(in)    :: this
    CHARACTER(LEN=MAX_CHAR), INTENT(out) :: variable
 
    variable = this%variable_string

  END SUBROUTINE get_variable_string

!---------------------------------------------------
  SUBROUTINE get_variable_logical(this, variable, error_out)
!---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(in)    :: this
    LOGICAL, INTENT(out)                 :: variable
    INTEGER, INTENT(out)                 :: error_out    

    error_out = 0

    IF ((this%variable_string == '.FALSE.') .OR. (this%variable_string == '.false.')) THEN
       variable = .FALSE.
    ELSE IF ((this%variable_string == '.TRUE.') .OR. (this%variable_string == '.true.')) THEN
       variable = .TRUE.
    ELSE
       WRITE(*,*)
       WRITE(*,*) '*** get variable error ***'
       WRITE(*,*) '*** Posible error: logical variables must be &
            written'
       WRITE(*,*) '    as .TRUE. or .FALSE.'
       WRITE(*,*) '    Please, check that. ***'
       WRITE(*,*) '*** Another possible error: this error happens when there is a '
       WRITE(*,*) '    tabulated space (invisible) in the input, behind the variables. '
       WRITE(*,*) '    Please, check that. ***'
       WRITE(*,*)
       error_out = 1
    ENDIF

  END SUBROUTINE get_variable_logical

!---------------------------------------------------
  SUBROUTINE get_variable_array_integer(this, variable, error_out)
!---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout)    :: this
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(out)   :: variable
    INTEGER, INTENT(out)                 :: error_out    
    INTEGER :: N_elem, error_int
    
    error_out = 0

    CALL get_format(this,5,error_int)
    IF (error_int == 1) THEN
       WRITE(*,*) 
       WRITE(*,*) '*** Input error: reading input error. ***'
       WRITE(*,*)
       error_out = 2
       GOTO 1000 !-- end of subroutine --
    ENDIF
    CALL take_size_array(this, N_elem)
    ALLOCATE(variable(N_elem))
    READ(this%variable_string, this%format, &
         iostat = error_out) variable
    IF (error_out .NE. 0) THEN
       WRITE(*,*) '*** get variable error ***'
       WRITE(*,*) '*** Possible error: this error happens when there '
       WRITE(*,*) '    is a tabulated space (invisible) behind the '
       WRITE(*,*) '    variables. Please, check that. ***'
    ENDIF

1000 CONTINUE

  END SUBROUTINE get_variable_array_integer

!---------------------------------------------------
  SUBROUTINE get_variable_array_real(this, variable, error_out)
!---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout)    :: this
    REAL*4, DIMENSION(:), ALLOCATABLE, INTENT(out)   :: variable
    INTEGER, INTENT(out)                 :: error_out    
    INTEGER :: N_elem, error_int
    
    error_out = 0

    CALL get_format(this,6,error_int)
    IF (error_int == 1) THEN
       WRITE(*,*) 
       WRITE(*,*) '*** Input error: reading input error. ***'
       WRITE(*,*)
       error_out = 2
       GOTO 1000 !-- end of subroutine --
    ENDIF
    CALL take_size_array(this, N_elem)
    ALLOCATE(variable(N_elem))
    READ(this%variable_string, this%format, &
         iostat = error_out) variable
    IF (error_out .NE. 0) THEN
       WRITE(*,*) '*** get variable error ***'
       WRITE(*,*) '*** Possible error: this error happens when there '
       WRITE(*,*) '    is a tabulated space (invisible) behind the '
       WRITE(*,*) '    variables. Please, check that. ***'
    ENDIF

1000 CONTINUE

  END SUBROUTINE get_variable_array_real

!---------------------------------------------------
  SUBROUTINE get_variable_array_double(this, variable, error_out)
!---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout)    :: this
    REAL*8, DIMENSION(:), ALLOCATABLE, INTENT(out)   :: variable
    INTEGER, INTENT(out)                 :: error_out    
    INTEGER :: N_elem, error_int
    
    error_out = 0

    CALL get_format(this,7,error_int)
    IF (error_int == 1) THEN
       WRITE(*,*) 
       WRITE(*,*) '*** Input error: reading input error. ***'
       WRITE(*,*)
       error_out = 2
       GOTO 1000 !-- end of subroutine --
    ENDIF
    CALL take_size_array(this, N_elem)
    ALLOCATE(variable(N_elem))
    READ(this%variable_string, this%format, &
         iostat = error_out) variable
    IF (error_out .NE. 0) THEN
       WRITE(*,*) '*** get variable error ***'
       WRITE(*,*) '*** Possible error: this error happens when there '
       WRITE(*,*) '    is a tabulated space (invisible) behind the '
       WRITE(*,*) '    variables. Please, check that. ***'
    ENDIF

1000 CONTINUE

  END SUBROUTINE get_variable_array_double
!------------------ END get_variable ------------------------------
