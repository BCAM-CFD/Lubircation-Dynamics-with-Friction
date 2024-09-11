!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------
SUBROUTINE write_potential_info(this, file)
  !----------------------------------------------------------
  use class_file
  IMPLICIT NONE
  TYPE(potential_type), INTENT(in) :: this
  TYPE(file_type), INTENT(in)      :: file

  !-- The file to be written with the information is opened --
  OPEN(file%unit, FILE=trim(file%name), ACCESS='APPEND')
  
  WRITE(file%unit,*) '--------- Potential info --------------'

  WRITE(file%unit,*) 'rcut   = ', this%rcut
  WRITE(file%unit,*) 'Ntable = ', this%Ntable
  WRITE(file%unit,*)

  CLOSE(file%unit)

END SUBROUTINE write_potential_info
