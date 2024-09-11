!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------------
SUBROUTINE initialize_physics(this, initial, file, unit, error_out)
  !-------------------------------------------------------
  use class_file
  IMPLICIT NONE
  TYPE(physics_type), INTENT(inout) :: this
  INTEGER, INTENT(in)               :: initial
  TYPE(file_type), INTENT(in)       :: file
  INTEGER, INTENT(in)               :: unit
  INTEGER, INTENT(out)              :: error_out
  INTEGER :: I

  error_out = 0

  !-- Particles are initialized --
  SELECT CASE(initial)
  CASE(1) !-- Random positions --
     CALL initialize_positions(this)
  CASE(2) !-- Positions read from a file --
     CALL read_pos(this, file, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF
  END SELECT

  !-- Other quantities are initialized --
  this%Rcut_sq = this%rcut**2.0

  WRITE(unit,*) '*** Physics initialized ***'

1000 CONTINUE

END SUBROUTINE initialize_physics
