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

!--------------------------------------------------
SUBROUTINE vel_iterative(this, time, error_out)
!--------------------------------------------------
  ! Adapted from MCF code.
  ! Implementation of the semi-implicit method.
  !------------------------------------------------
  use class_comp_time
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)      :: this
  TYPE(comp_time_type), INTENT(inout)   :: time
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: V0
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: V1
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: V_diff
  INTEGER, INTENT(out) :: error_out
  INTEGER  :: I, J
  INTEGER  :: N_sweep  
  INTEGER  :: N_sweep1
  INTEGER  :: N_sweep2
  LOGICAL  :: sweep_decrease
  LOGICAL  :: sweep_increase
  REAL(Pr) :: error_decrease
  REAL(Pr) :: error_increase
  REAL(Pr) :: sum_Vdiff
  REAL(Pr) :: sum_norm
  REAL(Pr) :: s
  REAL     :: t0, tf

  CALL cpu_time(t0)

  ALLOCATE(V0(this%N, this%dim))
  ALLOCATE(V1(this%N, this%dim))
  ALLOCATE(V_diff(this%N, this%dim))

  !-- The initial velocity is storaged --
  DO I = 1, this%N
     V0(I,:) = this%part(I)%vel(:)
  ENDDO

  !-- Implicit velocity is computed with N_sweep --
  N_sweep = this%N_sweep
  CALL vel_implicit_lub(this, N_sweep, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The calculated velocity is storaged --
  DO I = 1, this%N
     V1(I,:) = this%part(I)%vel(:)
  ENDDO
  
  N_sweep1       = N_sweep / 2
  sweep_decrease = .FALSE.
  error_decrease = 0.0_Pr !-- Necessary for starting the loop --
  DO WHILE (N_sweep1 >= 1 .AND. error_decrease < this%sweep_tol)
     !-- The initial velocity is recovered --
     DO I = 1, this%N
         this%part(I)%vel(:) = V0(I,:)
     ENDDO

     !-- Implicit velocity is computed with N_sweep1 --
     CALL vel_implicit_lub(this, N_sweep1, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF
     
     !-- Velocity difference is calculated --
     DO I = 1, this%N
        V_diff(I,:) = V1(I,:) - this%part(I)%vel(:)
     ENDDO
     
     sum_Vdiff = 0.0_Pr
     sum_norm  = 0.0_Pr
     DO I = 1, this%N
        DO J = 1, this%dim
           sum_Vdiff = sum_Vdiff + V_diff(I,J)**2.0_Pr
           sum_norm  = sum_norm  + V1(I,J)**2.0_Pr
        ENDDO
     ENDDO
     
     !-- Eq. (16) from JNNFM --
     error_decrease = SQRT(sum_Vdiff / sum_norm)

     !-- If error is small enough, we have to decrease --
     IF (error_decrease < this%sweep_tol) THEN
        N_sweep   = N_sweep1
        N_sweep1  = N_sweep / 2
        DO I = 1, this%N
           V1(I,:) = this%part(I)%vel(:)
        ENDDO
        sweep_decrease = .TRUE.
     ENDIF
  ENDDO
     
  IF (sweep_decrease) THEN
     
     !-- If number of sweeps was decreased,
     ! there is no need to perform increasing and
     ! save the current sutiable number of sweeps. --
     
     this%N_sweep = N_sweep     
     
  ELSE
     !-- If number of sweeps was not decreased,
     ! we need to increase the number of sweeps.
     ! There may be two reasons that we did not
     ! decrease number of sweeps.
     ! 1: N_sweep1 = 1 or
     ! 2: error_decrease > sweep_tolerance. --
     
     sweep_increase = .FALSE.
     N_sweep2 = N_sweep * 2
     
     IF (N_sweep1 >= 1) THEN
        !-- set initial error_increase to be error_decrease. --
        error_increase = error_decrease
     ELSE
        !-- set initial error to be an artibrarily big number --
        error_increase = 1.0e2_Pr
     ENDIF
     
     !-- perform number of sweeps until maximum 
     ! number or error is small enough. --
     DO  WHILE (N_sweep2 <= this%N_sweep_max .AND. &
          error_increase > this%sweep_tol) 
        !-- The initial velocity is recovered --
        DO I = 1, this%N
           this%part(I)%vel(:) = V0(I,:)
        ENDDO
        !-- Implicit velocity is computed with N_sweep2 --
        CALL vel_implicit_lub(this, N_sweep2, error_out)
        IF (error_out .NE. 0) THEN
           GOTO 1000 !-- End of subroutine --
        ENDIF
        
        !-- Velocity difference is calculated --
        DO I = 1, this%N
           V_diff(I,:) = V1(I,:) - this%part(I)%vel(:)
        ENDDO
        
        sum_Vdiff = 0.0_Pr
        sum_norm  = 0.0_Pr
        DO I = 1, this%N
           DO J = 1, this%dim
              sum_Vdiff = sum_Vdiff + V_diff(I,J)**2.0_Pr
              sum_norm  = sum_norm  + V1(I,J)**2.0_Pr
           ENDDO
        ENDDO
        
        !-- Eq. (16) from JNNFM --
        error_increase = SQRT(sum_Vdiff / sum_norm)
        
        IF (error_increase > this%sweep_tol) THEN
           N_sweep  = N_sweep2
           N_sweep2 = N_sweep * 2
           DO I = 1, this%N
              V1(I,:) = this%part(I)%vel(:)
           ENDDO
           sweep_increase = .TRUE.
           
        ENDIF
        
     ENDDO

     this%N_sweep = N_sweep     
  ENDIF
  
1000 CONTINUE
  
  IF (ALLOCATED(V0)) THEN
     DEALLOCATE(V0)
  ENDIF
  IF (ALLOCATED(V1)) THEN
     DEALLOCATE(V1)
  ENDIF
  IF (ALLOCATED(V_diff)) THEN
     DEALLOCATE(V_diff)
  ENDIF

  CALL cpu_time(tf) 
  time%semi_impl = time%semi_impl + tf - t0

END SUBROUTINE vel_iterative
