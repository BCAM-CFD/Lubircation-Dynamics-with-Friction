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

!---------------------- obtain_variable_string ------------------------------

!---------------------------------------------------
  SUBROUTINE obtain_variable_string(this)
!---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout) :: this
    INTEGER :: length, total_length
    CHARACTER(LEN = MAX_CHAR) :: line_aux

    length = this%length_name_variable
    total_length = len(this%line)

    line_aux = this%line(length + 1:total_length) !-- name of variable is deleted 
    line_aux = adjustl(line_aux)

    line_aux = line_aux(2:total_length) !-- = symbol is deleted --
    line_aux = adjustl(line_aux)
    
    this%variable_string = line_aux

  END SUBROUTINE obtain_variable_string

!------------------ END obtain_variable_string ------------------------------
