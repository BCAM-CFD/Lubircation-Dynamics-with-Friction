!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------
  SUBROUTINE update_name(this, step)
!-------------------------------------------------
    ! We update the name of the file with the 
    ! corresponding time step.
    !---------------------------------------------
    IMPLICIT NONE
    TYPE(file_type), INTENT(inout) :: this
    INTEGER, INTENT(in)            :: step
    CHARACTER(LEN=MAX_CHAR) :: termination

    WRITE(termination,'(I10.10,A)')  step,'.dat'
    this%name = trim(this%base_name)//trim(termination)

  END SUBROUTINE update_name
