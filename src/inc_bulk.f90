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
!---------------------------------------------------
SUBROUTINE bulk(this)
!---------------------------------------------------
  ! We decide which particles are considered as bulk.
  !-------------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER :: I, J, K, L, M

  DO I = 1, this%N
     IF (this%part(I)%pos(this%dim) > this%L_bulk .AND. &
          this%part(I)%pos(this%dim) < this%L(this%dim) - this%L_bulk) THEN
        this%part(I)%bulk = .TRUE.
     ELSE
        this%part(I)%bulk = .FALSE.
     ENDIF
  ENDDO


END SUBROUTINE bulk
