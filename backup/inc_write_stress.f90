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

!---------------------------------------------
SUBROUTINE write_stress(this, step)
  !---------------------------------------------
  ! Stress data is written out in the file "stress.dat".
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(in) :: this
  INTEGER, INTENT(in)           :: step

  IF (this%dim == 3) THEN
     WRITE(this%output%stress%unit, '(1I10, 9E20.10)')   &    
          step,                           &    !1
          this%Spp(1,:),                  &    !2  3  4
          this%Spp(2,:),                  &    !5  6  7
          this%Spp(3,:)                        !8  9  10
  ELSE !--- dim = 2 ---
     WRITE(this%output%stress%unit, '(1I10, 4E20.10)')   &    
          step,                           &    !1
          this%Spp(1,:),                  &    !2  3 
          this%Spp(2,:)                        !4  5
  ENDIF
  
  IF (this%dim == 3) THEN
     WRITE(this%output%stress_con%unit, '(1I10, 9E20.10)')   &    
          step,                               &    !1
          this%Spp_con(1,:),                  &    !2  3  4
          this%Spp_con(2,:),                  &    !5  6  7
          this%Spp_con(3,:)                        !8  9  10
  ELSE !--- dim = 2 ---
     WRITE(this%output%stress_con%unit, '(1I10, 4E20.10)')   &    
          step,                               &    !1
          this%Spp_con(1,:),                  &    !2  3 
          this%Spp_con(2,:)                        !4  5
  ENDIF
  
  IF (this%dim == 3) THEN
     WRITE(this%output%stress_lub%unit, '(1I10, 9E20.10)')   &    
          step,                               &    !1
          this%Spp_lub(1,:),                  &    !2  3  4
          this%Spp_lub(2,:),                  &    !5  6  7
          this%Spp_lub(3,:)                        !8  9  10
  ELSE !--- dim = 2 ---
     WRITE(this%output%stress_lub%unit, '(1I10, 4E20.10)')   &    
          step,                               &    !1
          this%Spp_lub(1,:),                  &    !2  3 
          this%Spp_lub(2,:)                        !4  5
  ENDIF
  
  IF (this%dim == 3) THEN
     WRITE(this%output%stress_rep%unit, '(1I10, 9E20.10)')   &    
          step,                               &    !1
          this%Spp_rep(1,:),                  &    !2  3  4
          this%Spp_rep(2,:),                  &    !5  6  7
          this%Spp_rep(3,:)                        !8  9  10
  ELSE !--- dim = 2 ---
     WRITE(this%output%stress_rep%unit, '(1I10, 4E20.10)')   &    
          step,                               &    !1
          this%Spp_rep(1,:),                  &    !2  3 
          this%Spp_rep(2,:)                        !4  5
  ENDIF
  
END SUBROUTINE write_stress
