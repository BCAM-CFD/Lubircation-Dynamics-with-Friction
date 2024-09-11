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
SUBROUTINE forces_explicit(this, error_out)
  !-----------------------------------
  ! Considering repulsion and lubrication forces explicitly.
  !-----------------------------------
  IMPLICIT NONE
  TYPE(system_type), intent(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I, J, K
  REAL(Pr) :: Rij_sq
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij(:)
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box(:)
  INTEGER :: dim
  REAL(Pr) :: Rij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij(:)
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vij(:)
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Flub(:)
  REAL(Pr) :: eij_vij
  REAL(Pr) :: F0
  REAL(Pr) :: exp_taus
  REAL(Pr) :: s
  REAL(Pr) :: R
  REAL(Pr) :: fij1
  REAL(Pr) :: fij2
  REAL(Pr) :: gij
  REAL(Pr) :: fiwall1
  REAL(Pr) :: giwall
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_forces_explicit.f90'  

  !-- Eqs (5) from JNNFM (for equal radius) --
  R = this%part(1)%R
  fij1 = -6.0_Pr * this%pi * this%eta0 * R**2.0_Pr / 4.0_Pr
  fij2 = -6.0_Pr * this%pi * this%eta0 * R * 9.0_Pr / 40.0_Pr
  gij   = -6.0_Pr * this%pi * this%eta0 * R * 20.0_Pr / 120.0_Pr

  !-- Similar than before for the interaction with walls (in this case fiwall2 is null) --
  fiwall1 = -6.0_Pr * this%pi * this%eta0 * R**2.0_Pr 
  giwall   = -6.0_Pr * this%pi * this%eta0 * R * 8.0_Pr / 15.0_Pr

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(vij(dim))
  ALLOCATE(Flub(dim))

  Box(:) = this%L(:)

  !-- Forces are put to zero --
  DO I = 1, this%N
     this%part(I)%force(:)     = 0.0_Pr
     this%wall%force_top(:)    = 0.0_Pr
     this%wall%force_bottom(:) = 0.0_Pr
  ENDDO

  !-- The forces are computed --
  DO I = 1, this%N
     DO K = 1, this%part(I)%N_neigh
        J = this%part(I)%neigh_list(K)

        !-- For now, we consider equal particles --
        IF (this%part(I)%R .NE. this%part(J)%R) THEN
           error_out = 1
           CALL error_header(file_name)           
           WRITE(*,*) '*** forces explicit error: for now, we are considering equal particles. ***'
           GOTO 1000 !-- End of subroutine --
        ENDIF
        
        include 'inc_calculate_Rij_sq.f90'

        IF (Rij_sq .LE. this%rcut_sq) THEN
           Rij = SQRT(Rij_sq) 
           eij(:) = pos_ij(:) / Rij
           IF (Rij < this%rcut_on) THEN
              Rij = this%rcut_on
           ENDIF

           s = Rij - this%part(I)%R - this%part(J)%R
           IF (s .LE. 0) THEN
              CALL error_header(file_name)              
              WRITE(*,*) '*** forces error: particles',I,'and', J,&
                   'are penetrating one to each other. ***'
              error_out = 1
              GOTO 1000 !-- End of subroutine --
           ENDIF

           !-- Repulsion force --
           exp_taus = EXP(-this%tau_rep * s)
           F0 = this%F0_rep * this%tau_rep * exp_taus / &
                (1.0_Pr - exp_taus) 
           this%part(I)%force(:) = this%part(I)%force(:) + &
                F0 * eij(:)
           this%part(J)%force(:) = this%part(J)%force(:) - &
                F0 * eij(:)
           vij(:) = this%part(I)%vel(:) - this%part(J)%vel(:)
           eij_vij = DOT_PRODUCT(eij, vij)

           !-- Lubrication force --
           Flub(:) = (fij1/s + fij2 * LOG(this%part(I)%R / s)) * eij_vij * eij(:) + &
                gij * LOG(this%part(I)%R / s) * (vij(:) - eij_vij * eij(:))
           this%part(I)%force(:) = this%part(I)%force(:) + Flub(:)
           this%part(J)%force(:) = this%part(J)%force(:) - Flub(:)
        ENDIF
     ENDDO

     !-- The forces from the walls --
     !-- Top wall --  i-particle   j-wall
     s   = this%L(dim) - this%part(I)%pos(dim) - this%part(I)%R
     IF (s .LE. 0) THEN
        CALL error_header(file_name)        
        WRITE(*,*) '*** forces error: top wall is being penetrated. ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
     IF (s .LE. (this%rcut - 2.0_Pr * this%part(I)%R)) THEN     
        !-- To avoid too big forces --
        IF (s .LE. (this%rcut_on - 2.0_Pr * this%part(I)%R)) THEN     
           s = this%rcut_on - 2.0_Pr * this%part(I)%R
        ENDIF
        eij(:)   = 0.0_Pr
        eij(dim) = -1.0_Pr
        vij(:) = this%part(I)%vel(:) - this%wall%vel_top(:)
        eij_vij = DOT_PRODUCT(eij, vij)
        
        !-- Repulsion force --
        exp_taus = EXP(-this%tau_rep * s)
        F0 = this%F0_rep * this%tau_rep * exp_taus / &
             (1.0_Pr - exp_taus) 
        this%part(I)%force(:) = this%part(I)%force(:) + &
             F0 * eij(:)
        this%wall%force_top(:) = this%wall%force_top(:) - &
             F0 * eij(:)
        
        !-- Lubrication force --
        Flub(:) = fiwall1/s * eij_vij * eij(:) + &
             giwall * LOG(this%part(I)%R / s) * (vij(:) - eij_vij * eij(:))
        this%part(I)%force(:) = this%part(I)%force(:) + &
             Flub(:)
        this%wall%force_top(:) = this%wall%force_top(:) - &
             Flub(:)
        
     ENDIF
     !-- Bottom wall --  i-particle   j-wall
     s   = this%part(I)%pos(dim) - this%part(I)%R
     IF (s .LE. 0) THEN
        CALL error_header(file_name)        
        WRITE(*,*) '*** forces error: top wall is being penetrated. ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
     
     IF (s .LE. (this%rcut - 2.0_Pr * this%part(I)%R)) THEN     
        !-- To avoid too big forces --
        IF (s .LE. (this%rcut_on - 2.0_Pr * this%part(I)%R)) THEN     
           s = this%rcut_on - this%part(I)%R
        ENDIF
        eij(:)   = 0.0_Pr
        eij(dim) = 1.0_Pr
        vij(:) = this%part(I)%vel(:) - this%wall%vel_bottom(:)
        eij_vij = DOT_PRODUCT(eij, vij)
        
        !-- Repulsion force --
        exp_taus = EXP(-this%tau_rep * s)
        F0 = this%F0_rep * this%tau_rep * exp_taus / &
             (1.0_Pr - exp_taus) 
        this%part(I)%force(:)     = this%part(I)%force(:)     + &
             F0 * eij(:)
        this%wall%force_bottom(:) = this%wall%force_bottom(:) - &
             F0 * eij(:)
        
        !-- Lubrication force --
        Flub(:) = fiwall1/s * eij_vij * eij(:) + &
             giwall * LOG(this%part(I)%R / s) * (vij(:) - eij_vij * eij(:))
        this%part(I)%force(:) = this%part(I)%force(:) + &
             Flub(:)
        this%wall%force_bottom(:) = this%wall%force_bottom(:) - &
             Flub(:)
     ENDIF
  ENDDO

  ! --- Accelerations of the particles are calculated ---
  DO I = 1, this%N
     this%part(I)%acc(:) = this%part(I)%force(:) / this%part(I)%mass
  ENDDO

1000 CONTINUE

  IF (ALLOCATED(Box)) THEN
     DEALLOCATE(Box)
  ENDIF
  IF (ALLOCATED(pos_ij)) THEN
     DEALLOCATE(pos_ij)
  ENDIF
  IF (ALLOCATED(eij)) THEN
     DEALLOCATE(eij)
  ENDIF
  IF (ALLOCATED(vij)) THEN
     DEALLOCATE(vij)
  ENDIF
  IF (ALLOCATED(Flub)) THEN
     DEALLOCATE(Flub)
  ENDIF

END SUBROUTINE forces_explicit
