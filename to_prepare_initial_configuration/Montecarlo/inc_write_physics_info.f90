!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------
SUBROUTINE write_physics_info(this, file)
  !----------------------------------------------------------
  use class_file
  IMPLICIT NONE
  TYPE(physics_type), INTENT(in) :: this
  TYPE(file_type), INTENT(in)    :: file
  CHARACTER(LEN=MAX_CHAR) :: formatting

  IF (SIZE(this%box) == 2) THEN
     formatting = '(A, 2E20.10)'
  ELSE
     formatting = '(A, 3E20.10)'
  ENDIF

  !-- The file to be written with the information is opened --
  OPEN(file%unit, FILE=trim(file%name), ACCESS='APPEND')
  
  WRITE(file%unit,*) '--------- Physics info --------------'

  WRITE(file%unit,*) 'N_part  = ', this%N_part
  WRITE(file%unit,*) 'dim     = ', this%dim
  WRITE(file%unit, formatting) ' Box     = ', this%Box
  WRITE(file%unit,*) 'kT      = ', this%kT
  WRITE(file%unit,*) 'rcut    = ', this%rcut
  WRITE(file%unit,*) 'rcut_sq = ', this%rcut_sq
  WRITE(file%unit,*)

  CLOSE(file%unit)

END SUBROUTINE write_physics_info
