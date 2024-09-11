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

!---------------- read_input_constructor ------------------
  
  !--------------------------------------------------
  SUBROUTINE read_input_constructor(this, file, variable_name)
  !--------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout)          :: this
    CHARACTER(LEN=MAX_CHAR), OPTIONAL, INTENT(in) :: file
    CHARACTER(LEN=MAX_CHAR), OPTIONAL, INTENT(in) :: variable_name

    IF (present(file)) THEN
       this%file = file
    ELSE
       this%file = 'input'
    ENDIF
    this%variable_name = variable_name

  END SUBROUTINE read_input_constructor

!-------------END read_input_constructor ------------------
