!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------
SUBROUTINE write_montecarlo(this, file, step, error_out)
!------------------------------------------
  ! This subroutine writes some quantities about
  ! the montecarlo method in a file.
  !----------------------------------------
  use class_file
  IMPLICIT NONE
  TYPE(montecarlo_type), INTENT(In) :: this
  TYPE(file_type), INTENT(in)       :: file
  INTEGER, INTENT(in)               :: step
  INTEGER, INTENT(out)              :: error_out
  LOGICAL :: opened

  INQUIRE(unit=file%unit, opened = opened) !-- If the system is using it --
  IF (.NOT.(opened)) THEN
     WRITE(*,*) '*** write_montecarlo error: the unit for the file is not opened. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The montecarlo quantities are written in the file ---
  WRITE(file%unit, '(I15, 2E20.10)') &
       step,            & ! 1
       this%acceptance, & ! 2
       this%amplitude     ! 3

1000 CONTINUE
  
END SUBROUTINE write_montecarlo
