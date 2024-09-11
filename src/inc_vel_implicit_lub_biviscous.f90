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
!-------------------------------------------------
SUBROUTINE vel_implicit_lub_biviscous(this, N_sweep, error_out)
  !-------------------------------------------------
  ! The implicit system between pairs of particles is solved.  
  ! Following Vazquez-Quesada et al, Journal of Non-Newtonian
  ! Fluid Mechanics, 2016.
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)              :: N_sweep
  INTEGER, INTENT(out)             :: error_out
  REAL(Pr) :: dt_sweep
  INTEGER  :: T
  INTEGER  :: I, J, K
  REAL(Pr) :: R
  REAL(Pr) :: fij1
  REAL(Pr) :: fij2
  REAL(Pr) :: gij
  REAL(Pr) :: fi_wall1
  REAL(Pr) :: fi_wall2
  REAL(Pr) :: fi_wall3
  REAL(Pr) :: giwall
  REAL(Pr) :: h
  REAL(Pr) :: Rij_sq
  REAL(Pr) :: rij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vi_old
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vj_old
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vij_old
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vij_new
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Fij
  INTEGER  :: dim
  REAL(Pr) :: eij_vij_old
  REAL(Pr) :: massr_inv
  REAL(Pr) :: massr_inv_dt
  REAL(Pr) :: Aij
  REAL(Pr) :: Bij
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  
  REAL(Pr) :: gamma_dot_s_max
  REAL(Pr) :: h0_lim
  REAL(Pr) :: h0
  REAL(Pr) :: rmax
  REAL(Pr) :: gmgc
  REAL(Pr) :: P
  REAL(Pr) :: Q
  REAL(Pr) :: r1
  REAL(Pr) :: r2
  REAL(Pr) :: h_r1
  REAL(Pr) :: h_r2
  
  error_out = 0

  file_name = 'inc_vel_implicit_lub_biviscous.f90'  

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(vi_old(dim))
  ALLOCATE(vj_old(dim))
  ALLOCATE(vij_old(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(vij_new(dim))
  ALLOCATE(Fij(dim))

  !-- Lubrication forces on walls are only calculated in the last substep --
  this%wall%lub_force_top(:)    = 0.0_Pr
  this%wall%lub_force_bottom(:) = 0.0_Pr

  Box(:) = this%L(:)

  dt_sweep = this%dt / REAL(N_sweep, KIND = Pr)
  !-- inverse of the reduced mass is computed --
  massr_inv_dt = (2.0_Pr / this%part(1)%mass) * dt_sweep
  
  !-- Eqs (5) from JNNFM (for equal radius) --
  R = this%part(1)%R
  fij1 = -6.0_Pr * this%pi * this%eta0 * R**2.0_Pr / 4.0_Pr * massr_inv_dt
  fij2 = -6.0_Pr * this%pi * this%eta0 * R * 9.0_Pr / 40.0_Pr * massr_inv_dt
  gij  = -6.0_Pr * this%pi * this%eta0 * R / 6.0_Pr * massr_inv_dt

  !-- Similar than before for the interaction with walls (in this case fi_wall2 is null) --
  fi_wall1 = -6.0_Pr * this%pi * this%eta0 * R**2.0_Pr
  fi_wall2 = -6.0_Pr/5.0_Pr * this%pi * this%eta0 * R
  fi_wall3 = -6.0_Pr * this%pi * this%eta0 * R * 0.971264_Pr
  giwall   = -6.0_Pr * this%pi * this%eta0 * R * 8.0_Pr / 15.0_Pr 

  DO T = 1, N_sweep
     DO I = 1, this%N

!!$        !-- For now, we consider equal particles --
!!$        IF (this%part(I)%R .NE. R) THEN
!!$           error_out = 1
!!$           CALL error_header(file_name)        
!!$           WRITE(*,*) '*** vel implicit lub error: we are considering only particles &
!!$                with the same radius. ***'
!!$           GOTO 1000 !-- End of subroutine --
!!$        ENDIF
        
        DO K = 1, this%part(I)%N_neigh
           J = this%part(I)%neigh_list(K)

           !*** This distance can be calculated before (only once) for more 
           !    efficiency in an array similar to neigh_list ***
           include 'inc_calculate_Rij_sq.f90'
           IF (Rij_sq .LE. this%rcut_sq) THEN
              Rij = SQRT(Rij_sq)
              !-- The old velocity is storaged --
              vi_old(:)   = this%part(I)%vel(:)
              vj_old(:)   = this%part(J)%vel(:)

              vij_old(:)  = vi_old(:) - vj_old(:)

              eij(:)      = pos_ij(:) / Rij
              eij_vij_old = DOT_PRODUCT(eij, vij_old)
              IF (Rij < this%rcut_on) THEN
                 Rij = this%rcut_on
              ENDIF

              !-- The gap between particles is calculated --
!              h   = Rij - this%part(I)%R - this%part(J)%R
              h   = Rij - 2.0_Pr * R
              IF (h .LE. 0) THEN
                 CALL error_header(file_name)                 
                 WRITE(*,*) '*** vel implicit error: particles',I,'and', J,&
                      'are penetrating one to each other. ***'
                 error_out = 1
                 GOTO 1000 !-- End of subroutine --
              ENDIF

                           
              !WRITE(*,*) '*** lub ***'
				gamma_dot_s_max = ABS(9.0_Pr / 8.0_Pr * ((eij_vij_old)/h) * SQRT((3.0_Pr / 2.0_Pr) * (this%a_par / h)) )
				
				h0_lim = 3.0_Pr/4.0_Pr*((9.0_Pr/2.0_Pr)*eij_vij_old**2.0_Pr*this%a_par/(this%gamma_dot_critical**2.0_Pr))**(1.0_Pr/3.0_Pr)
				h0 = h
				rmax = SQRT(2.0_Pr/3.0_Pr*this%a_par*h0)
				gmgc= gamma_dot_s_max/this%gamma_dot_critical
				P = (-1.0_Pr + (2.0_Pr*gmgc**2.0_Pr) + (2.0_Pr*gmgc*SQRT(-1.0_Pr + gmgc**2.0_Pr)))**(1.0_Pr/3.0_Pr)
				Q = SQRT(-1.0_Pr + (1.0_Pr / P)  +  P)
				r1 = rmax*(Q- SQRT(-3.0_Pr - Q**2.0_Pr  + (4.0_Pr * gmgc/ Q) ) )
				r2 = rmax*(Q+ SQRT(-3.0_Pr - Q**2.0_Pr  + (4.0_Pr * gmgc/ Q) ) )
				
				h_r1 = h0*(1.0_Pr + (r1**2.0_Pr / (2.0_Pr*this%a_par*h0)) )
				h_r2 = h0*(1.0_Pr + (r2**2.0_Pr / (2.0_Pr*this%a_par*h0)) )
				
				!###### The force is given by the forces written below as
				!#    F = Fm0,  if h0 >= h0_lim
				!#    F = Fb,   if h0 < h0_lim
				!# Fm1 is not used
!				#########################################################
				fij1=0.0_Pr
				!WRITE(*,*) h0_lim, h0
				IF (h0 .GE. h0_lim ) THEN	
					!# Monoviscous force with eta = eta0
					fij1 = 6.0_Pr * this%pi * eij_vij_old * this%a_par**2.0 * this%eta0 / h0
					!WRITE(*,*) '*** here ***', eij_vij_old, fij1
					
					!# Monoviscous force with eta = eta1
					!Fm1(h0, gamma_dot) = 6.0 * pi * V(gamma_dot) * a**2.0 * eta1(eta_ratio0) * h0**(-1.0)
				ELSE
				    !WRITE(*,*) '*** NO here ***', h0_lim, h0
					!# Biviscous Force
					!fij1 = 6.0_Pr * this%pi * eij_vij_old * this%a_par**2.0 * this%eta0 / h0
					
					fij1 = 2.0_Pr * this%pi * ( (3.0_Pr * this%a_par**2.0 * this%eta0 * eij_vij_old * (1.0_Pr/ h0)) + &
					(3.0_Pr * (this%eta0 - this%eta1) * eij_vij_old * this%a_par**2.0 * &
					(h_r2**(-1.0) - h_r1**(-1.0)) * (2.0 - h0 * (h_r1**(-1.0) + h_r2**(-1.0))) ) + &
					(2.0 * this%a_par * this%gamma_dot_critical * (this%eta0 - this%eta1) * &
					( (r2- r1) + ( (h_r1- 2.0 * h0) * SQRT(2.0 * this%a_par/h0) * &
				    (ATAN(r2/SQRT(2.0 * this%a_par * h0)) - ATAN(r1/ SQRT(2.0 * this%a_par * h0)) ) ) ) ) )
				    !WRITE(*,*)  eij_vij_old, fij1
                ENDIF
              
               !-- Eq (27) from Comp. Part. Mech. --
              Aij = -fij1 * massr_inv_dt
              Bij = 0.0_Pr

              !-- Eqs (14) and (15) from JNNFM --
              vij_new(:) = 1.0_Pr / (1.0_Pr - Bij) * (vij_old(:) + (Aij - Bij) / (1.0_Pr - Aij) * eij(:))

!!$              this%part(J)%vel(:) = (this%part(I)%mass * &
!!$                   (vi_old(:) - vij_new(:)) + &
!!$                   this%part(J)%mass * vj_old(:)) / &
!!$                   (this%part(I)%mass + this%part(J)%mass)

              this%part(J)%vel(:) = 0.5_Pr * &
                   (vi_old(:) - vij_new(:) +  vj_old(:)) 

              this%part(I)%vel(:) = vij_new(:) + this%part(J)%vel(:)

           ENDIF
           
        ENDDO
     ENDDO

     !--- Now the walls. 
     !    Because of the velocity imposition, the system of equations to solve
     !    does not need to conserve linear momentum
     !-- Top wall --  i-particle   j-wall
     DO I = 1, this%N
        !-- The gap is calculated --
        h   = this%L(dim) - this%part(I)%pos(dim) - this%part(I)%R
        IF (h .LE. 0) THEN
           CALL error_header(file_name)           
           WRITE(*,*) '*** vel implicit error: top wall is being penetrated. ***'
           error_out = 1
           GOTO 1000 !-- End of subroutine --
        ENDIF
        IF (h .LE. (this%rcut - 2.0_Pr * this%part(I)%R)) THEN
           !-- To avoid too big forces --
           IF (h < (this%rcut_on - 2.0_Pr * this%part(I)%R)) THEN     
              h = this%rcut_on - 2.0_Pr * this%part(I)%R
           ENDIF
           vi_old(:)   = this%part(I)%vel(:)
           vj_old(:)   = this%wall%vel_top(:)
           vij_old(:)  = vi_old(:) - vj_old(:)
           eij(:)      = 0.0_Pr
           eij(dim)    = -1.0_Pr
           eij_vij_old = DOT_PRODUCT(eij, vij_old)
           
           !-- inverse of the reduced mass is computed --
           massr_inv = 1.0_Pr / this%part(I)%mass !-- Note that the wall does 
                                                  ! not contribute --
           
           !-- Eqs (13) from JNNFM --
           Aij = (fi_wall1 / h + fi_wall2 * LOG(this%part(I)%R / h) + &
                fi_wall3)* massr_inv * dt_sweep
                
			IF (h .LE. ((this%rcut- 2.0_Pr * this%part(I)%R)/5.0_Pr) ) THEN
				Bij = giwall * LOG(this%part(I)%R / h) * massr_inv * dt_sweep
			ELSE
				Bij = 0.0_Pr
			ENDIF     
           
           
           !-- Eq (14) from JNNFM --
           vij_new(:) = 1.0_Pr / (1.0_Pr - Bij) * &
                (vij_old(:) + (Aij - Bij) / (1.0_Pr - Aij) * &
                eij_vij_old * eij(:))
           
           this%part(I)%vel(:) = vij_new(:) + vj_old(:)              
           
           !-- lubrication force on wall is calculated --
           IF (T == N_sweep) THEN
            Fij(:) = (vij_new(:) - vij_old(:)) / (dt_sweep * massr_inv)
       this%wall%lub_force_top(:) = this%wall%lub_force_top(:) - Fij(:)
           ENDIF
        ENDIF
     ENDDO

     !-- Bottom wall --  i-particle   j-wall
     DO I = 1, this%N
        !-- The gap is calculated --
        h   = this%part(I)%pos(dim) - this%part(I)%R
        IF (h .LE. 0) THEN
           CALL error_header(file_name)           
           WRITE(*,*) '*** vel implicit error: bottom wall is being penetrated. ***'
           error_out = 1
           GOTO 1000 !-- End of subroutine --
        ENDIF
        IF (h .LE. (this%rcut - 2.0_Pr * this%part(I)%R)) THEN
           !-- To avoid too big forces --
           IF (h < (this%rcut_on - 2.0_Pr * this%part(I)%R)) THEN     
              h = this%rcut_on - 2.0_Pr * this%part(I)%R
           ENDIF
           vi_old(:)   = this%part(I)%vel(:)
           vj_old(:)   = this%wall%vel_bottom(:)
           vij_old(:)  = vi_old(:) - vj_old(:)
           eij(:)      = 0.0_Pr
           eij(dim)    = 1.0_Pr
           eij_vij_old = DOT_PRODUCT(eij, vij_old)
           
           !-- inverse of the reduced mass is computed --
           massr_inv = 1.0_Pr / this%part(I)%mass  !-- Note that the wall does 
                                                   ! not contribute -- 
           
           !-- Eqs (13) from JNNFM --
           Aij = (fi_wall1 / h + fi_wall2 * LOG(this%part(I)%R / h) + &
                fi_wall3)* massr_inv * dt_sweep
                
			IF (h .LE. ((this%rcut- 2.0_Pr * this%part(I)%R)/5.0_Pr) ) THEN
				Bij = giwall * LOG(this%part(I)%R / h) * massr_inv * dt_sweep
			ELSE
				Bij = 0.0_Pr
			ENDIF  
           
           
           !-- Eq (14) from JNNFM --
           vij_new(:) = 1.0_Pr / (1.0_Pr - Bij) * &
                (vij_old(:) + (Aij - Bij) / (1.0_Pr - Aij) * &
                eij_vij_old * eij(:))
           
           this%part(I)%vel(:) = vij_new(:) + vj_old(:)
           
           !-- lubrication force on wall is calculated --
           IF (T == N_sweep) THEN
             Fij(:) = (vij_new(:) - vij_old(:)) / (dt_sweep * massr_inv)
              this%wall%lub_force_bottom(:) = &
                   this%wall%lub_force_bottom(:) - Fij(:)
           ENDIF
        ENDIF
     ENDDO
  ENDDO

1000 CONTINUE

  !-- Memory is realeased --
  IF (ALLOCATED(pos_ij)) THEN
     DEALLOCATE(pos_ij)
  ENDIF
  IF (ALLOCATED(Box)) THEN
     DEALLOCATE(Box)
  ENDIF
  IF (ALLOCATED(vi_old)) THEN
     DEALLOCATE(vi_old)
  ENDIF
  IF (ALLOCATED(vj_old)) THEN
     DEALLOCATE(vj_old)
  ENDIF
  IF (ALLOCATED(vij_old)) THEN
     DEALLOCATE(vij_old)
  ENDIF
  IF (ALLOCATED(eij)) THEN
     DEALLOCATE(eij)
  ENDIF
  IF (ALLOCATED(vij_new)) THEN
     DEALLOCATE(vij_new)
  ENDIF
  IF (ALLOCATED(Fij)) THEN
     DEALLOCATE(Fij)
  ENDIF

END SUBROUTINE vel_implicit_lub_biviscous
