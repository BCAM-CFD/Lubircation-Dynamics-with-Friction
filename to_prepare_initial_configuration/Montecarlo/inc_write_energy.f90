!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------------------------------------------------
SUBROUTINE write_energy(energy, file, step, error_out)
  !--------------------------------------------------------
  ! Some macro quantities are computed and written.
  !----------------------------------------------------
  use class_file
  IMPLICIT NONE
  REAL(Pr), INTENT(in)        :: energy
  TYPE(file_type), INTENT(in) :: file
  INTEGER, INTENT(in)         :: step
  INTEGER, INTENT(out)        :: error_out
  logical :: opened

  INQUIRE(unit=file%unit, opened = opened) !-- If the system is using it --
  IF (.NOT.(opened)) THEN
     WRITE(*,*) '*** write_energy error: the unit for the file is not opened. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  WRITE(file%unit, *) step, &  ! 1
       energy                  ! 2

1000 CONTINUE

END SUBROUTINE write_energy
