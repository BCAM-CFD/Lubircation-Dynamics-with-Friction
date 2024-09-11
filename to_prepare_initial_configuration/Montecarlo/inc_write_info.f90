!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------------
SUBROUTINE write_info(this, file)
!-------------------------------------------------------
  IMPLICIT NONE
  TYPE(sim_type), INTENT(in)  :: this
  TYPE(file_type), INTENT(in) :: file

  OPEN(file%unit, FILE=trim(file%name), &
       FORM='FORMATTED', STATUS='UNKNOWN')
  WRITE(file%unit,*) '************************* Simulation info ***********************'
  CLOSE(file%unit)

  !-- Info about files is written --
  WRITE(file%unit,*)
  WRITE(file%unit,*) '--------- Files info --------------'
  CLOSE(file%unit)
  CALL write_all_files_info(this, file)

  !--- Physics info ----
  CALL write_physics_info(this%physics, file)

  !--- Potential info ----
  CALL write_potential_info(this%potential, file)

  !--- random info ---
  CALL write_random_info(this%random, file)

  !--- montecarlo info ---
  CALL write_MC_info(this%montecarlo, file)

  !--- cell system info ---
  CALL write_cell_system_info(this%cells, file)

END SUBROUTINE write_info
