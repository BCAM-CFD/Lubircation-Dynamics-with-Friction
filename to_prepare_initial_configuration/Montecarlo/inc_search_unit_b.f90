!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!*********************************************
!  CLASS READ_INPUT FILE
!*********************************************

!----------------------------------------------------
SUBROUTINE search_unit_b(unit, error_out)
!----------------------------------------------------
  ! A usable unit for files is searched.
  ! This subroutine was taken from class_files_utilities.
  ! That is why the name is a bit different, in order
  ! to avoid incompatibilities if both classes are in 
  ! the same code.
  !--------------------------------------------------
  IMPLICIT NONE
  INTEGER, INTENT(out)   :: unit
  INTEGER, INTENT(inout) :: error_out
  INTEGER :: max_unit
  INTEGER :: I
  LOGICAL :: opened

  error_out = 0
  max_unit  = 1000

  DO I = 1, max_unit
     INQUIRE(unit=I, opened = opened) !-- If the system is using it --
     IF (.NOT.(opened)) THEN
        unit = I
        GOTO 456
     ENDIF
  ENDDO
  !-- If this line is reached, no free unit was found --
  WRITE(*,*) '*** Search unit error: no free unit has been found. ***'
  error_out = 1
456 CONTINUE

END SUBROUTINE search_unit_b
