!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE write_initial(this, error_out)
!----------------------------------------------------------------------------
  ! Some files are written in the beginning of the simulation
  !--------------------------------------------------------------------------
  IMPLICIT NONE
  TYPE(sim_type), INTENT(inout) :: this
  INTEGER, INTENT(out)       :: error_out
  INTEGER :: I

  error_out = 0

  I = 0
  CALL write_info(this, this%file(3))
!  CALL write_potential_table(this%potential, this%file(5))
  CALL write_potential(this%potential, this%file(5), 10000)
  CALL write_pos(this%physics, this%file(1), I)
  CALL write_energy(this%physics%energy, this%file(2), I, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

1000 CONTINUE

END SUBROUTINE write_initial
