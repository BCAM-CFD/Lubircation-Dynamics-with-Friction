!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------
SUBROUTINE write_MC_info(this, file)
  !----------------------------------------------------------
  use class_file
  IMPLICIT NONE
  TYPE(montecarlo_type), INTENT(in) :: this
  TYPE(file_type), INTENT(in)       :: file
  CHARACTER(LEN=MAX_CHAR) :: formatting

  !-- The file to be written with the information is opened --
  OPEN(file%unit, FILE=trim(file%name), ACCESS='APPEND')
  
  WRITE(file%unit,*) '--------- Montecarlo info --------------'

  WRITE(file%unit,*) 'N_steps_check    = ', this%N_steps_check
  WRITE(file%unit,*) 'N_freq_stat      = ', this%N_freq_stat
  WRITE(file%unit,*) 'amplitude        = ', this%amplitude
  WRITE(file%unit,*) 'min_amp          = ', this%min_amp
  WRITE(file%unit,*) 'max_amp          = ', this%max_amp
  WRITE(file%unit,*) 'acceptance ratio = ', this%acceptance_ratio
  WRITE(file%unit,*) 'beta             = ', this%beta
  WRITE(file%unit,*)

  CLOSE(file%unit)

END SUBROUTINE write_MC_info
