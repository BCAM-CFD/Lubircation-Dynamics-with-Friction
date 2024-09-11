!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------
SUBROUTINE write_file_info(this, file)
  !----------------------------------------------------------
  IMPLICIT NONE
  TYPE(file_type), INTENT(in) :: this
  TYPE(file_type), INTENT(in) :: file
  LOGICAL :: opened

  !-- The file to be written with the information is opened --
  OPEN(file%unit, FILE=trim(file%name), ACCESS='APPEND')

  !-- We check if it is opened --
  INQUIRE(unit=this%unit, opened = opened) 

  WRITE(file%unit,*) 'Assigned unit ', this%unit
  WRITE(file%unit,*) 'File name     ', trim(this%name)
  IF (opened) THEN
     WRITE(file%unit,*) 'unit opened'
  ELSE
     WRITE(file%unit,*) 'unit closed'
  ENDIF
  WRITE(file%unit,*)

  CLOSE(file%unit)

END SUBROUTINE write_file_info
