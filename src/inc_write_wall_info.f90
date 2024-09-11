!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Sagaya Savarimuthu Prasanna Kumar, from Marco Ellero's group.
! - Jose Antonio Ruiz-Lopez, from Marco Ellero's group.
!    Currently from the Department of Applied Physics at
!    Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Sagaya Savarimuthu Prasanna Kumar.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------
!------------------------------------------------------------
SUBROUTINE write_wall_info(this, file, error_out)
  !----------------------------------------------------------
  ! Info about the wall object is written in the file "info.dat".
  !-----------------------------------------------------------
  use class_file
  use class_files_utilities
  IMPLICIT NONE
  TYPE(wall_type), INTENT(in) :: this
  TYPE(file_type), INTENT(in)   :: file
  INTEGER, INTENT(out)          :: error_out
  INTEGER :: unit
  CHARACTER(LEN=50) :: form !-- because of ambiguity I can not use MAX_CHAR
  INTEGER :: dim

  !-- We look for a free unit number --
  CALL search_unit(unit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  dim = SIZE(this%vel_top)

  IF (dim == 2) THEN
     form = '(A, 2F10.5)'
  ELSE
     form = '(A, 3F10.5)'
  ENDIF

  !-- The file to be written with the information is opened --
  OPEN(unit, FILE=trim(file%name), ACCESS='APPEND')

  WRITE(unit,*)   
  WRITE(unit,*) '--------- wall info --------------'

  WRITE(unit,form) ' Top Vwall    = ', this%vel_top
  WRITE(unit,form) ' Bottom Vwall = ', this%vel_bottom
  WRITE(unit,*)

  CLOSE(unit)

1000 CONTINUE

END SUBROUTINE write_wall_info
