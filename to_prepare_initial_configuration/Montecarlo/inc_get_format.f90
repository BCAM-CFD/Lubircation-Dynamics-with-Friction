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

!------------------------------ get_format ----------------

  !-----------------------------------------
  SUBROUTINE get_format(this, type_data, error_out)
    !---------------------------------------
    ! type_data = 1  --> real
    ! type_data = 2  --> double
    ! type_data = 3  --> integer
    ! type_data = 4  --> string
    ! type_data = 5  --> array of integers    
    ! type_data = 6  --> array of reals
    ! type_data = 7  --> array of doubles
    !---------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout) :: this
    INTEGER, INTENT(in)                  :: type_data
    INTEGER, INTENT(out), OPTIONAL       :: error_out
    INTEGER :: length, length2, pos_point
    CHARACTER(LEN=MAX_CHAR) :: length_str, length2_str
    CHARACTER(LEN=MAX_CHAR) :: format_int
    INTEGER :: N_elem
    INTEGER :: pos1, pos2, I, counter
    CHARACTER(LEN=MAX_CHAR) :: str_aux
    
    IF (present(error_out)) THEN
       error_out = 0
    ENDIF

    format_int = '(1I10)'
    
    length = len_trim(this%variable_string)
    WRITE(length_str, format_int) length
    length_str = adjustl(length_str)

    pos_point = SCAN(this%variable_string, ".")
    IF ((type_data == 1) .OR. (type_data == 2)) THEN
       IF (pos_point .NE. 0) THEN
          length2 = length - pos_point
       ELSE
          length2 = 0
       ENDIF
       WRITE(length2_str, format_int) length2
       length2_str = adjustl(length2_str)
    ELSE IF (type_data == 3) THEN
       IF (pos_point .NE. 0) THEN
          IF (present(error_out)) THEN
             error_out = 2 !-- A real is written instead of an integer
             GOTO 1000 !-- end of subroutine --
          ENDIF
       ENDIF
    ENDIF

    SELECT CASE(type_data)
       CASE(1) ! real
          this%format = &
               '(1F'//trim(length_str)//'.'//trim(length2_str)//')'

       CASE(2) ! double
          this%format = &
               '(1D'//trim(length_str)//'.'//trim(length2_str)//')'

       CASE(3) ! integer
          this%format = '(1I'//trim(length_str)//')'          

       CASE(4) ! string
          this%format = '(1A'//trim(length_str)//')'          

       CASE(5) ! array of integers 
          CALL take_size_array(this, N_elem)
          this%format = '('
          counter = 1
          pos1 = 1
          DO I = 1, len_trim(this%variable_string) + 1
             IF ((this%variable_string(I:I) == ' ') .AND. (this%variable_String(I+1:I+1) .NE. ' ') .OR. &
                (this%variable_string(I:I) .NE. ' ') .AND. (I == len_trim(this%variable_string))) then
                pos2 = I
                length = pos2 - pos1 + 1
                WRITE(length_str, format_int) length
                length_str = adjustl(length_str)

                this%format = trim(this%format)//'1I'//trim(length_str)
                IF (counter < N_elem) THEN
                   this%format = trim(this%format)//','
                ENDIF
                counter = counter + 1
                pos1 = pos2 + 1
             ENDIF
          ENDDO
          this%format = trim(this%format)//')'

       CASE(6, 7) ! array of reals or doubles
          CALL take_size_array(this, N_elem)
          this%format = '('
          counter = 1
          pos1 = 1
          DO I = 1, len_trim(this%variable_string) + 1
             IF ((this%variable_string(I:I) == ' ') .AND. (this%variable_String(I+1:I+1) .NE. ' ') .OR. &
                (this%variable_string(I:I) .NE. ' ') .AND. (I == len_trim(this%variable_string))) then
                pos2 = I
                str_aux = this%variable_string(pos1:pos2)
                length = pos2 - pos1 + 1
                WRITE(length_str, format_int) length
                length_str = adjustl(length_str)

                pos_point = SCAN(str_aux, ".")
                IF (pos_point .NE. 0) THEN
                   length2 = length - pos_point
                ELSE
                   length2 = 0
                ENDIF
                WRITE(length2_str, format_int) length2
                length2_str = adjustl(length2_str)

                IF (type_data == 6) THEN
                   this%format = trim(this%format)//'1F'//trim(length_str)//'.'//trim(length2_str)
                ELSE
                   this%format = trim(this%format)//'1D'//trim(length_str)//'.'//trim(length2_str)
                ENDIF
                
                IF (counter < N_elem) THEN
                   this%format = trim(this%format)//','
                ENDIF
                counter = counter + 1
                pos1 = pos2 + 1
             ENDIF
          ENDDO
          this%format = trim(this%format)//')'

       CASE DEFAULT
          IF (present(error_out)) THEN
             error_out = 1
          ENDIF
       END SELECT

1000 CONTINUE

  END SUBROUTINE get_format

!-------------------------- END get_format ----------------
