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

!-------------------------------------------
SUBROUTINE error_header(file_name)
!-------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: file_name
  
  WRITE(*,*) 
  WRITE(*,*) '----------------------------------'
  WRITE(*,*) 'Error in file ', trim(file_name)
  WRITE(*,*) '----------------------------------'

END SUBROUTINE error_header
