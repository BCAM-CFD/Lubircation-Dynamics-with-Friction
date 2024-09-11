!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Sagaya Savarimuthu Prasanna Kumar, from Marco Ellero's group.
! - Jose Antonio Ruiz-Lopez, from Marco Ellero's group.
!    Currently from the Department of Applied Physics at
!    Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Sagaya Savarimuthu Prasanna Kumar.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------
!-----------------------------------------------------
SUBROUTINE destroy_output(this)
  !-----------------------------------------------------
  ! Destructor of the class output
  !---------------------------------------------------
  IMPLICIT NONE
  TYPE(output_type), INTENT(inout) :: this
  LOGICAL :: opened

  INQUIRE(unit=this%info%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%info%unit)
  ENDIF

  INQUIRE(unit=this%particles%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%particles%unit)
  ENDIF
  
  INQUIRE(unit=this%walls%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%walls%unit)
  ENDIF

  INQUIRE(unit=this%Nsweeps%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%Nsweeps%unit)
  ENDIF

  INQUIRE(unit=this%stress%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%stress%unit)
  ENDIF
  
  INQUIRE(unit=this%stress_con%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%stress_con%unit)
  ENDIF
  
    INQUIRE(unit=this%stress_rep%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%stress_rep%unit)
  ENDIF
  
    INQUIRE(unit=this%stress_lub%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%stress_lub%unit)
  ENDIF

  INQUIRE(unit=this%comp_time%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%comp_time%unit)
  ENDIF

  INQUIRE(unit=this%shear_rate%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%shear_rate%unit)
  ENDIF

END SUBROUTINE destroy_output
