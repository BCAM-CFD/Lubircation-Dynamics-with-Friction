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

!------------------------------------------------------------
SUBROUTINE write_random_info(this, file, error_out)
  !----------------------------------------------------------
  ! The information about the class random is written out in "info.dat".
  !----------------------------------------------------------
  use class_file
  use class_files_utilities
  IMPLICIT NONE
  TYPE(random_type), INTENT(in) :: this
  TYPE(file_type), INTENT(in)   :: file
  INTEGER, INTENT(out)          :: error_out
  INTEGER :: unit

  !-- We look for a free unit number --
  CALL search_unit(unit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The file to be written with the information is opened --
  OPEN(unit, FILE=trim(file%name), ACCESS='APPEND')

  WRITE(unit,*)   
  WRITE(unit,*) '--------- random info --------------'

  WRITE(unit,*) 'fixed_seed = ', this%fixed_seed
  WRITE(unit,*) 'seed       = ', this%seed
  WRITE(unit,*)

  CLOSE(unit)

1000 CONTINUE

END SUBROUTINE write_random_info
