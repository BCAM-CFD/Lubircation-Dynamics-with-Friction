!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!   Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Jose Antonio Ruiz Lopez, currently from the Department of applied physics at
!   Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!   in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------

!*********************************************
!  CLASS READ_INPUT FILE
!*********************************************

!--------------------- take_size_array ---------------------
! Size of arrays in input_file can be inquired here.
!-----------------------------------------------------------

!--------------------------------------------------------
  SUBROUTINE take_size_array(this, N)
    !--------------------------------------------------------
    ! Subroutine to calculate the size of an array written
    ! in an input file.
    !--------------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(in) :: this
    INTEGER, INTENT(out)              :: N
    INTEGER :: I

    N = 1
    DO I = 2, len_trim(this%variable_string) - 1
       IF ((this%variable_string(I:I) == ' ') .AND. (this%variable_string(I-1:I-1) .NE. ' ')) THEN
          N = N + 1
       ENDIF
    ENDDO

  END SUBROUTINE take_size_array
