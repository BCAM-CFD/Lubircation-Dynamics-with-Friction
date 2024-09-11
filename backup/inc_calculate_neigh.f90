!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!   Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Jose Antonio Ruiz Lopez, currently from the Department of applied physics at
!   Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!   in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------

!-----------------------------------
SUBROUTINE calculate_neigh(this, time, error_out)
  !-----------------------------------
  ! It is checked if the neighbours list needs to be updated. If so, the job is done.
  !-----------------------------------
  use class_comp_time
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)    :: this
  TYPE(comp_time_type), INTENT(inout) :: time
  INTEGER, INTENT(out)                :: error_out
  REAL :: t0, tf
  INTEGER :: I

  CALL cpu_time(t0)

  error_out = 0

  CALL check_update_neigh(this)

  !-- If it is necessary the neighbours list is updated --
  IF (this%update_neigh) THEN
     CALL calculate_cells_list(this, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF
     CALL calculate_neigh_list(this, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF
     DO I = 1, this%N
        this%part(I)%pos0(:) = this%part(I)%pos(:)
     ENDDO
  ENDIF

1000 CONTINUE

  CALL cpu_time(tf)
  time%neigh = time%neigh + tf - t0

END SUBROUTINE calculate_neigh
