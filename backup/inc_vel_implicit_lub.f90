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

!-------------------------------------------------
SUBROUTINE vel_implicit_lub(this, N_sweep, error_out)
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
  REAL(Pr) :: h, hinv
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
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: sij, u1, u2
  INTEGER  :: dim
  REAL(Pr) :: mi, mj, massr_inv_dt
  REAL(Pr) :: Imomj, Ij_inv_dt, pieta
  REAL(Pr) :: eij_vij_old, eij_sij
  REAL(Pr) :: massr_inv
  REAL(Pr) :: Aij
  REAL(Pr) :: Bij
  REAL(Pr) :: l, l1, l3, l5, lf5, f, g1, g2
  REAL(Pr) :: ai, aj, am
  REAL(Pr) :: a_sq
  REAL(Pr) :: a_sh
  REAL(Pr) :: a_pu
  REAL(Pr) :: a_ro
  REAL(Pr) :: A_ab
  REAL(Pr) :: B_ab
  REAL(Pr) :: C_ab
  REAL(Pr) :: D_ab
  REAL(Pr) :: E_ab
  REAL(Pr) :: F_ab
  REAL(Pr) :: G_ab
  REAL(Pr) :: H_ab
  REAL(Pr) :: K_ab
  REAL(Pr) :: L_ab
  REAL(Pr) :: M_ab
  REAL(Pr) :: W_ab
  REAL(Pr) :: det
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eCrossOmega_ij, eCrossOmegaij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Omegaij    !a-b
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Omega_ij   !a+b
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij_Omegaij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij_Omega_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eijCrossVij
  
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  
  
  error_out = 0

  file_name = 'inc_vel_implicit_lub.f90'  

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(vi_old(dim))
  ALLOCATE(vj_old(dim))
  ALLOCATE(vij_old(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(vij_new(dim))
  ALLOCATE(Fij(dim))

  ALLOCATE(eCrossOmega_ij(dim))
  ALLOCATE(eCrossOmegaij(dim))
  ALLOCATE(Omegaij(dim))
  ALLOCATE(Omega_ij(dim))
  ALLOCATE(eij_Omegaij(dim))
  ALLOCATE(eij_Omega_ij(dim))
  ALLOCATE(eijCrossVij(dim))
  ALLOCATE(sij(dim), u1(dim), u2(dim))

  !-- Lubrication forces on walls are only calculated in the last substep --
  this%wall%lub_force_top(:)    = 0.0_Pr
  this%wall%lub_force_bottom(:) = 0.0_Pr

  Box(:) = this%L(:)

  dt_sweep = this%dt / REAL(N_sweep, KIND = Pr)
  !-- inverse of the reduced mass is computed --
  
  !-- Eqs (5) from JNNFM (for equal radius) --
  
  DO T = 1, N_sweep
     DO I = 1, this%N
     
     ai = this%part(I)%R
     mi = this%part(I)%mass
     pieta = this%pi * this%eta0 * ai        
        DO K = 1, this%part(I)%N_neigh
           J = this%part(I)%neigh_list(K)
           aj = this%part(J)%R
           am = 0.5_Pr * (ai + aj)
           include 'inc_calculate_Rij_sq.f90'
           IF (Rij_sq .LE. this%rcut_sq*am*am) THEN           
              mj = this%part(J)%mass
              massr_inv_dt = ((1.0_Pr/mi) + (1.0_Pr/mj)) * dt_sweep
              l  = aj/ai
              l1 = 1.0_Pr + l
              l3 = l1 * l1 * l1
              l5 = l * l * l * l * l
              
              f = l*(ai + (4.0_Pr * aj))/((4.0_Pr*ai) + aj)
              g1 = ((l5 * ((4.0_Pr/l) - 1.0_Pr)) - (4.0_Pr*l) + 1.0_Pr)/2.0_Pr
              g2 = ((l5 * ((4.0_Pr/l) - 1.0_Pr)) + (4.0_Pr*l) - 1.0_Pr)/2.0_Pr
              lf5 = l5 + f
              Imomj = 2.0_Pr * mj * aj * aj  / 5.0_Pr
              
              Ij_inv_dt = dt_sweep/Imomj
              
              Rij = SQRT(Rij_sq)
              !-- The old velocity is storaged --
              vi_old(:)   = this%part(I)%vel(:)
              vj_old(:)   = this%part(J)%vel(:)

              vij_old(:)  = vi_old(:) - vj_old(:)

              eij(:)      = pos_ij(:) / Rij
              eij_vij_old = DOT_PRODUCT(eij, vij_old)


              !-- The gap between particles is calculated --
              h   = (Rij - ai - aj)/am

              !IF (h .LE. 0) THEN
                ! CALL error_header(file_name)                 
                ! WRITE(*,*) '*** vel implicit error: particles',I,'and', J,&
                !      'are penetrating one to each other. ***'
                ! error_out = 1
                 !CYCLE
              IF (h.LE. 2.0_Pr*(this%rough/am)) THEN
                 h = 2.0_Pr*this%rough/am
              ENDIF
              hinv = 1.0_Pr/h
              
              
              a_sq = 6.0_Pr * pieta * ( (2.0_Pr*l*l* hinv/l3) &
                     + ((l*(1.0_Pr + (7.0_Pr*l) + (l*l))* LOG(hinv))/(5.0_Pr*l3 )))
              a_sh = (8.0_Pr/5.0_Pr) * pieta * LOG(hinv) * (l*(2.0_Pr + l + (2.0_Pr *l*l))/l3) 
              a_ro = (4.0_Pr/5.0_Pr) * pieta * ai * l * (4.0_Pr + l) * LOG(hinv)/(l1*l1)
              a_pu = 4.0_Pr * pieta * ai * ai * l * l * LOG(hinv)/ (l1*5.0_Pr)
              
              A_ab = massr_inv_dt * (a_sq - a_sh)
              B_ab = 1.0_Pr + (massr_inv_dt * a_sh)
              C_ab = am * massr_inv_dt * a_sh
              D_ab = massr_inv_dt * a_ro * (1.0_Pr - f)*0.5_Pr
              
              E_ab = - Ij_inv_dt * am * a_ro * lf5
              F_ab = 1.0_Pr + (Ij_inv_dt * am * a_ro * lf5)
              G_ab = - Ij_inv_dt * a_ro * lf5
              H_ab = Ij_inv_dt * g1 * a_pu
              
              K_ab = - Ij_inv_dt * g2 * a_pu
              L_ab = 1.0_Pr + (Ij_inv_dt * g2 * a_pu)
              M_ab = - Ij_inv_dt * a_ro * (l5-f)
              
              det = (F_ab * L_ab) + (am * M_ab * H_ab)
              
              W_ab = ((C_ab * ((L_ab*G_ab) - (H_ab*M_ab))) + (D_ab*M_ab*(F_ab+(am*G_ab))))/det

	      Omegaij(:)     = this%part(I)%Omega(:) - this%part(J)%Omega(:)  
          Omega_ij(:)    = this%part(I)%Omega(:) + this%part(J)%Omega(:)                                              
          eCrossOmega_ij(1) = eij(2)*Omega_ij(3) - eij(3)*Omega_ij(2)
	      eCrossOmega_ij(2) = eij(3)*Omega_ij(1) - eij(1)*Omega_ij(3)
	      eCrossOmega_ij(3) = eij(1)*Omega_ij(2) - eij(2)*Omega_ij(1)
	      
	      eCrossOmegaij(1) = eij(2)*Omegaij(3) - eij(3)*Omegaij(2)
	      eCrossOmegaij(2) = eij(3)*Omegaij(1) - eij(1)*Omegaij(3)
	      eCrossOmegaij(3) = eij(1)*Omegaij(2) - eij(2)*Omegaij(1)
	      
	      sij(:) = vij_old(:) - ((((C_ab*L_ab) + (am *D_ab*M_ab))/det)*eCrossOmega_ij(:)) &
	      + ((((C_ab*H_ab) - (D_ab*F_ab))/det)*eCrossOmegaij(:))
	    
		eij_sij = DOT_PRODUCT(eij, sij)
		eij_Omegaij(:) = DOT_PRODUCT(eij, Omegaij)*eij(:)
		eij_Omega_ij(:) = DOT_PRODUCT(eij, Omega_ij)*eij(:)

        vij_new(:) = (((W_ab-A_ab)/((A_ab + B_ab)*(B_ab + W_ab))) * eij_sij * eij(:)) &
        + ((1.0_Pr / (B_ab + W_ab))* sij(:))

		this%part(J)%vel(:) = (1.0_Pr/((l*l*l)+1.0_Pr))*(vi_old(:)+(l*l*l*vj_old(:))-vij_new(:))
        this%part(I)%vel(:) = vij_new(:) + this%part(J)%vel(:) 
                
		!IF(I==1 .AND. K==1) THEN
		!WRITE(* , *) W_ab, A_ab, B_ab, det, sij, this%part(I)%vel(:),this%part(K)%vel(:)
        !ENDIF
		eijCrossVij(1) = eij(2)*vij_new(3) - eij(3)*vij_new(2)
		eijCrossVij(2) = eij(3)*vij_new(1) - eij(1)*vij_new(3)
		eijCrossVij(3) = eij(1)*vij_new(2) - eij(2)*vij_new(1)
    
		
		u1(:)= Omega_ij(:) - ((E_ab/(E_ab+F_ab))*eij_Omega_ij(:))&
		- (G_ab*eijCrossVij(:)) + ((H_ab/(K_ab+L_ab))*eij_Omegaij(:))
		
		u2(:) = Omegaij(:) - ((K_ab/(K_ab+L_ab))*eij_Omegaij(:)) - (M_ab*eijCrossVij(:))&
		- (((am*M_ab)/(E_ab + F_ab))*eij_Omega_ij(:))
		
		this%part(I)%Omega(:) = 0.5_Pr*(((L_ab+(am*M_ab))*u1(:)) + ((F_ab-H_ab)*u2(:)))/det
		this%part(J)%Omega(:) = 0.5_Pr*(((L_ab-(am*M_ab))*u1(:)) - ((F_ab+H_ab)*u2(:)))/det            
           
        ENDIF
        
        ENDDO
     ENDDO
     !WRITE(*, *) this%part(1)%vel(:), this%part(1)%Omega(:) 

     !--- Now the walls. 
     !    Because of the velocity imposition, the system of equations to solve
     !    does not need to conserve linear momentum
     !-- Top wall --  i-particle   j-wall
     
     DO I = 1, this%N
        !-- The gap is calculated --
        ai  = this%part(I)%R
        h   = this%L(dim) - this%part(I)%pos(dim) - ai
        Rij = h + ai 
        IF (h .LE. 0) THEN
           CALL error_header(file_name)           
           WRITE(*,*) '*** vel implicit error: top wall is being penetrated. ***'
           error_out = 1
           GOTO 1000 !-- End of subroutine --
        ENDIF

        IF (Rij .LE. (this%rcut - 2.0_Pr + ai)) THEN
           !-- To avoid too big forces --
           IF (h < this%rcut_on) THEN     
              h = this%rcut_on
           ENDIF
           
           hinv = 1.0_Pr/h
   

  !-- Similar than before for the interaction with walls (in this case fi_wall2 is null) --
  fi_wall1 = -6.0_Pr * this%pi * this%eta0 * ai**2.0_Pr
  fi_wall2 = -(6.0_Pr/5.0_Pr) * this%pi * this%eta0 * ai
  fi_wall3 = -6.0_Pr * this%pi * this%eta0 * ai * 0.971264_Pr
  giwall   = -6.0_Pr * this%pi * this%eta0 * ai * 8.0_Pr / 15.0_Pr 

           
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
           Aij = ((fi_wall1*hinv) + (fi_wall2 * LOG(this%part(I)%R*hinv)) + &
                fi_wall3)* massr_inv * dt_sweep
           Bij = giwall * LOG(this%part(I)%R*hinv) * massr_inv * dt_sweep
   
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
        ai=this%part(I)%R
        !-- Similar than before for the interaction with walls (in this case fi_wall2 is null) --
  fi_wall1 = -6.0_Pr * this%pi * this%eta0 * ai**2.0_Pr
  fi_wall2 = -(6.0_Pr/5.0_Pr) * this%pi * this%eta0 * ai
  fi_wall3 = -6.0_Pr * this%pi * this%eta0 * ai * 0.971264_Pr
  giwall   = -6.0_Pr * this%pi * this%eta0 * ai * 8.0_Pr / 15.0_Pr 
        h   = this%part(I)%pos(dim) - ai

        IF (h .LE. 0) THEN
           CALL error_header(file_name)           
           WRITE(*,*) '*** vel implicit error: bottom wall is being penetrated. ***'
           error_out = 1
           GOTO 1000 !-- End of subroutine --
        ENDIF
        
        IF (h .LE. (this%rcut - 2.0_Pr)) THEN
           !-- To avoid too big forces --
           IF (h < (this%rcut_on)) THEN     
              h = this%rcut_on
           ENDIF
           
           hinv = 1.0_Pr/h
           
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
           Aij = ((fi_wall1 * hinv) + (fi_wall2 * LOG(this%part(I)%R*hinv)) + &
                fi_wall3)* massr_inv * dt_sweep
           Bij = giwall * LOG(this%part(I)%R*hinv) * massr_inv * dt_sweep
           
           
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
 IF (ALLOCATED(eCrossOmega_ij)) THEN
     DEALLOCATE(eCrossOmega_ij)
  ENDIF
  IF (ALLOCATED(eCrossOmegaij)) THEN
     DEALLOCATE(eCrossOmegaij)
  ENDIF
 IF (ALLOCATED(Omegaij)) THEN
     DEALLOCATE(Omegaij)
  ENDIF
 IF (ALLOCATED(Omega_ij)) THEN
     DEALLOCATE(Omega_ij)
  ENDIF
IF (ALLOCATED(eij_Omegaij)) THEN
     DEALLOCATE(eij_Omegaij)
  ENDIF
IF (ALLOCATED(eij_Omega_ij)) THEN
     DEALLOCATE(eij_Omega_ij)
  ENDIF
IF (ALLOCATED(eijCrossVij)) THEN
     DEALLOCATE(eijCrossVij)
  ENDIF
  IF (ALLOCATED(sij)) THEN
     DEALLOCATE(sij)
  ENDIF
  IF (ALLOCATED(u1)) THEN
     DEALLOCATE(u1)
  ENDIF
  IF (ALLOCATED(u2)) THEN
     DEALLOCATE(u2)
  ENDIF
  

END SUBROUTINE vel_implicit_lub
