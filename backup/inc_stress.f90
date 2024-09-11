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
SUBROUTINE stress(this, error_out)
  !-----------------------------------
  ! The average stress tensor on the particles in the bulk is 
  ! computed in order to calculate the normal stress diferences.
  ! The stress is calculated with the Irving-Kirkwood method.
  ! Check Bertevas et al, Rheological acta, 2010 and
  ! Phan-Thien et al, Journal of rheology, 2014.
  ! **** Previously to compute the stress, the particles
  ! from the bulk should be calculated ***
  !-----------------------------------
  IMPLICIT NONE
  TYPE(system_type), intent(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I, J, K, M
  REAL(Pr) :: Rij_sq
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box
  INTEGER :: dim
  REAL(Pr) :: Rij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij, tij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Fij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vi
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: defij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Flub, Frep, Fcon
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Flub_norm
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Flub_tang
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_con, Spp_lub, Spp_rep
  REAL(Pr) :: l, l1, l3, f
  REAL(Pr) :: ai, aj, am, ar
  REAL(Pr) :: pieta
  REAL(Pr) :: eij_vij
  REAL(Pr) :: F0, Fn, Ft
  REAL(Pr) :: exp_taus
  REAL(Pr) :: s
  REAL(Pr) :: h, hinv
  REAL(Pr) :: R
  REAL(Pr) :: def
  REAL(Pr) :: fij1
  REAL(Pr) :: fij2
  REAL(Pr) :: gij
  REAL(Pr) :: Rij0 !dummy variable to temp. store Rij
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
  REAL(Pr) :: a_sq
  REAL(Pr) :: a_sh
  REAL(Pr) :: a_pu
  REAL(Pr) :: a_ro
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eCrossOmega_ij, eCrossOmegaij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Omega_ij, Omegaij   !a+b
  REAL(Pr) :: deltaij

  error_out = 0

  file_name = 'inc_stress.f90'  

  !-- Eqs (5) from JNNFM (for equal radius) --
  R = this%part(1)%R
  !fij1 = -6.0_Pr * this%pi * this%eta0 * R**2.0_Pr / 4.0_Pr
  !fij2 = -6.0_Pr * this%pi * this%eta0 * R * 9.0_Pr / 40.0_Pr
  !gij  = -6.0_Pr * this%pi * this%eta0 * R / 6.0_Pr

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(Fij(dim))
  ALLOCATE(vij(dim))
  ALLOCATE(vi(dim))
  ALLOCATE(defij(dim))
  ALLOCATE(tij(dim))
  ALLOCATE(Flub(dim), Fcon(dim), Frep(dim))
  ALLOCATE(Flub_norm(dim))
  ALLOCATE(Flub_tang(dim))
  ALLOCATE(Spp(dim,dim))
  ALLOCATE(Spp_rep(dim,dim), Spp_con(dim,dim), Spp_lub(dim,dim))
  ALLOCATE(eCrossOmega_ij(dim), eCrossOmegaij(dim))
  ALLOCATE(Omega_ij(dim), Omegaij(dim))

  Box(:) = this%L(:)

  !-- Forces are initialized ---
  DO I = 1, this%N
     this%part(I)%Spp(:,:) = 0.0_Pr
     this%part(I)%Spp_con(:,:) = 0.0_Pr
     this%part(I)%Spp_rep(:,:) = 0.0_Pr
     this%part(I)%Spp_lub_norm(:,:) = 0.0_Pr
  ENDDO

  !-- Forces and stresses are calculated --
  DO I = 1, this%N
  
  ai = this%part(I)%R
  pieta = this%pi * this%eta0 * ai
  
     DO K = 1, this%part(I)%N_neigh
        
        J = this%part(I)%neigh_list(K)
        aj = this%part(J)%R
        am = 0.5_Pr * (ai + aj)
               
        Fij(:)  = 0.0_Pr     !-- Interparticle force initialized --
        Fcon(:) = 0.0_Pr     !-- Interparticle contact force initialized --
        Flub(:) = 0.0_Pr     !-- Interparticle lubrication force initialized --
        Frep(:) = 0.0_Pr     !-- Interparticle repulsive force initialized
        
        IF ((this%part(I)%bulk) .OR. (this%part(J)%bulk)) THEN

           include 'inc_calculate_Rij_sq.f90'

           IF (Rij_sq .LE. this%rcut_sq*am*am) THEN
           
              ar = ai*aj/am
              Rij = SQRT(Rij_sq) 
              eij(:) = pos_ij(:) / Rij
              vij(:) = this%part(I)%vel(:) - this%part(J)%vel(:)
              eij_vij = DOT_PRODUCT(eij, vij)
              
              s = (Rij - ai - aj)/am
              deltaij = 2.0_Pr * this%rough/am
              
              
              IF (s .LE. deltaij) THEN !-- Friction Contact
                 
                 !CALL error_header(file_name)                 
                 !WRITE(*,*) '*** stress error: particles',I,'and', J,&
                 !     'are penetrating one to each other. ***'
                 !error_out = 1
                 !GOTO 1000 !-- End of subroutine --
                
                !Normal Friction force
                
                h = - s
                
                Fn = this%Fcn*(h+deltaij)
                Fcon(:)= Fn * eij(:)

                defij(:)=this%part(I)%def_tan(K,:)
                def=DOT_PRODUCT(defij,defij)
                def=sqrt(def)
                
                tij(:)=defij(:)/def
                Ft=this%Fct*def
                Fcon(:) = Fcon(:) - Ft*tij(:)

                exp_taus = EXP(-this%tau_rep * s)
                exp_taus = min(exp_taus, 1.0_Pr)
                 
                Frep(:) = this%F0_rep * ar * exp_taus * eij(:)
                
                
              hinv = 1.0_Pr/deltaij
              l= aj/ai
              l1 = 1.0_Pr + l
              l3 = l1 * l1 * l1
              f  = l*(ai + (4.0_Pr * aj))/((4.0_Pr*ai) + aj)

              a_sq = 6.0_Pr * pieta * ((2.0_Pr*l*l*hinv/l3)&
                     + ((l*(1.0_Pr + (7.0_Pr*l) + (l*l))* LOG(hinv))/(5.0_Pr*l3 )))
              a_sh = (8.0_Pr/5.0_Pr) * pieta * (l*(2.0_Pr + l + (2.0_Pr *l*l))/l3) * LOG(hinv)
              a_ro = (4.0_Pr/5.0_Pr) * pieta * ai * l * (4.0_Pr + l) * LOG(hinv)/(l1*l1)
              a_pu = 4.0_Pr * pieta * ai * ai * l * l * LOG(hinv)/ (l1*5.0_Pr)

            Omega_ij(:)       = this%part(I)%Omega(:) + this%part(J)%Omega(:)
            Omegaij(:)        = this%part(I)%Omega(:) - this%part(J)%Omega(:)
            eCrossOmega_ij(1) = eij(2)*Omega_ij(3) - eij(3)*Omega_ij(2)
            eCrossOmega_ij(2) = eij(3)*Omega_ij(1) - eij(1)*Omega_ij(3)
            eCrossOmega_ij(3) = eij(1)*Omega_ij(2) - eij(2)*Omega_ij(1)

            eCrossOmegaij(1) = eij(2)*Omegaij(3) - eij(3)*Omegaij(2)
            eCrossOmegaij(2) = eij(3)*Omegaij(1) - eij(1)*Omegaij(3)
            eCrossOmegaij(3) = eij(1)*Omegaij(2) - eij(2)*Omegaij(1)

            Flub(:) = -(a_sq*eij_vij*eij(:)) - (a_sh*(vij(:) - (eij_vij*eij(:)))) - &
                  (am*a_sh*eCrossOmega_ij(:)) - (0.5*(1-f)*a_ro*eCrossOmegaij(:))
                
                
                
                 
              ELSE !-- Lubricated contact --
              
              h = s
              

                
              hinv = 1.0_Pr/h
              l= aj/ai
              l1 = 1.0_Pr + l
              l3 = l1 * l1 * l1
              f  = l*(ai + (4.0_Pr * aj))/((4.0_Pr*ai) + aj)
              
              a_sq = 6.0_Pr * pieta * ((2.0_Pr*l*l*hinv/l3)&
                     + ((l*(1.0_Pr + (7.0_Pr*l) + (l*l))* LOG(hinv))/(5.0_Pr*l3 )))
              a_sh = (8.0_Pr/5.0_Pr) * pieta * (l*(2.0_Pr + l + (2.0_Pr *l*l))/l3) * LOG(hinv)
              a_ro = (4.0_Pr/5.0_Pr) * pieta * ai * l * (4.0_Pr + l) * LOG(hinv)/(l1*l1)
              a_pu = 4.0_Pr * pieta * ai * ai * l * l * LOG(hinv)/ (l1*5.0_Pr)
	      
            Omega_ij(:)       = this%part(I)%Omega(:) + this%part(J)%Omega(:)
            Omegaij(:)        = this%part(I)%Omega(:) - this%part(J)%Omega(:)
            eCrossOmega_ij(1) = eij(2)*Omega_ij(3) - eij(3)*Omega_ij(2)
            eCrossOmega_ij(2) = eij(3)*Omega_ij(1) - eij(1)*Omega_ij(3)
            eCrossOmega_ij(3) = eij(1)*Omega_ij(2) - eij(2)*Omega_ij(1)
            
            eCrossOmegaij(1) = eij(2)*Omegaij(3) - eij(3)*Omegaij(2)
            eCrossOmegaij(2) = eij(3)*Omegaij(1) - eij(1)*Omegaij(3)
            eCrossOmegaij(3) = eij(1)*Omegaij(2) - eij(2)*Omegaij(1)

            Flub(:) = -(a_sq*eij_vij*eij(:)) - (a_sh*(vij(:) - (eij_vij*eij(:)))) - &
                  (am*a_sh*eCrossOmega_ij(:)) - (0.5*(1-f)*a_ro*eCrossOmegaij(:))
        
            exp_taus = EXP(-this%tau_rep * s)
            Frep(:)= this%F0_rep * ar * exp_taus * eij(:) 
            
            !IF (I==1) THEN
		     !  write(*,*) I, J, Frep(:),s, exp_taus
			!ENDIF
            
             
			ENDIF	
        !-- Lubrication force --
        !Flub_norm(:) = (fij1/s + fij2 * LOG(this%part(I)%R / s)) * eij_vij * eij(:) 
                  
		!IF (s .LE. ((this%rcut- 2.0_Pr * this%part(I)%R)/5.0_Pr) ) THEN
		!	Flub_tang(:) = gij * LOG(this%part(I)%R / s) * (vij(:) - eij_vij * eij(:))
		!ELSE
		!	Flub_tang(:) = 0.0_Pr
		!ENDIF
		!Flub_norm(:) = -(fij1) * eij(:) 
		!WRITE(* , *) fij1, Flub_norm(1)				
		!Flub_tang(:) = 0.0_Pr              
                   
        !-- Replusion force --
        
        !IF(s .LE. ((this%rcut_on- 2.0_Pr * this%part(I)%R)*0.00001_Pr))THEN
        !      s = (this%rcut_on- 2.0_Pr * this%part(I)%R)*0.00001_Pr
        !ENDIF
              
!        IF (s .LE. 0) THEN
!            CALL error_header(file_name)                 
!            WRITE(*,*) '*** stress error: particles',I,'and', J,&
!                'are penetrating one to each other. ***'
!            error_out = 1
!            GOTO 1000 !-- End of subroutine --
!        ENDIF
              
         Fij(:) = Fcon(:)+Flub(:)+Frep(:)     

              
              
              DO M = 1, dim
                 Spp_con(M,:) = pos_ij(M) * Fcon(:)
                 Spp_lub(M,:) = pos_ij(M) * Flub(:)
                 Spp_rep(M,:) = pos_ij(M) * Frep(:)
                 Spp(M,:) = pos_ij(M) * Fij(:)
              ENDDO
              
              this%part(I)%Spp_con(:,:) = this%part(I)%Spp_con(:,:) + Spp_con(:,:)
              this%part(J)%Spp_con(:,:) = this%part(J)%Spp_con(:,:) + Spp_con(:,:)
              
              this%part(I)%Spp_lub_norm(:,:) = this%part(I)%Spp_lub_norm(:,:) &
              + Spp_lub(:,:)
              this%part(J)%Spp_lub_norm(:,:) = this%part(J)%Spp_lub_norm(:,:) &
              + Spp_lub(:,:)
              
              this%part(I)%Spp_rep(:,:) = this%part(I)%Spp_rep(:,:) + Spp_rep(:,:)
              this%part(J)%Spp_rep(:,:) = this%part(J)%Spp_rep(:,:) + Spp_rep(:,:)
              
              this%part(I)%Spp(:,:) = this%part(I)%Spp(:,:) + Spp(:,:)
              this%part(J)%Spp(:,:) = this%part(J)%Spp(:,:) + Spp(:,:)
              
           ENDIF
        ENDIF

     ENDDO
  ENDDO

  !-- The stress tensor is calculated ---
  this%Spp(:,:) = 0.0_Pr
  this%Spp_con(:,:) = 0.0_Pr
  this%Spp_rep(:,:) = 0.0_Pr
  this%Spp_lub(:,:) = 0.0_Pr
  DO I = 1, this%N
     IF (this%part(I)%bulk) THEN
        vi(:) = this%part(I)%vel(:)
        vi(1) = vi(1) - ((-0.5_Pr*this%L(dim)*this%calc_gamma_dot) + &
             this%calc_gamma_dot * this%part(I)%pos(dim)) 
        DO J = 1, dim
           Spp(J,:) = vi(J) * vi(:) * this%part(I)%mass 
        ENDDO
        
        this%Spp_con(:,:) = this%Spp_con(:,:) + &
             (0.5_Pr * this%part(I)%Spp_con(:,:))
             
        this%Spp_lub(:,:) = this%Spp_lub(:,:) + &
             (0.5_Pr * this%part(I)%Spp_lub_norm(:,:))
             
        this%Spp_rep(:,:) = this%Spp_rep(:,:) + &
             (0.5_Pr * this%part(I)%Spp_rep(:,:))
        
        this%Spp(:,:) = this%Spp(:,:) + &
             (0.5_Pr * this%part(I)%Spp(:,:)) + Spp(:,:)
     ENDIF
  ENDDO
  this%Spp_con = -this%Spp_con / this%V_bulk
  this%Spp_lub = -this%Spp_lub / this%V_bulk
  this%Spp_rep = -this%Spp_rep / this%V_bulk
  this%Spp = -this%Spp / this%V_bulk
  
!   OPEN(UNIT=89, FILE= 'test/Fij.dat', ACTION="write", STATUS="replace")
!   DO I=1,N_part-1
! 	 WRITE(89,'(8E20.10)') R0_ij(I), F_rep_mag(I), F_nor_mag(I), F_tan_mag(I), F_tot_mag(I), F_ref_mag(I), I_part(I), J_part(I)
!   ENDDO
!   CLOSE(UNIT=89)
  
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
  IF (ALLOCATED(Fij)) THEN
     DEALLOCATE(Fij)
  ENDIF
  IF (ALLOCATED(vij)) THEN
     DEALLOCATE(vij)
  ENDIF
  IF (ALLOCATED(defij)) THEN
     DEALLOCATE(defij)
  ENDIF
  IF (ALLOCATED(tij)) THEN
     DEALLOCATE(tij)
  ENDIF
  IF (ALLOCATED(vi)) THEN
     DEALLOCATE(vi)
  ENDIF
  IF (ALLOCATED(Flub)) THEN
     DEALLOCATE(Flub)
  ENDIF
  IF (ALLOCATED(Fcon)) THEN
     DEALLOCATE(Fcon)
  ENDIF
  IF (ALLOCATED(Frep)) THEN
     DEALLOCATE(Frep)
  ENDIF
  IF (ALLOCATED(Flub_norm)) THEN
     DEALLOCATE(Flub_norm)
  ENDIF
  IF (ALLOCATED(Flub_tang)) THEN
     DEALLOCATE(Flub_tang)
  ENDIF
  IF (ALLOCATED(Spp)) THEN
     DEALLOCATE(Spp)
  ENDIF
    IF (ALLOCATED(Spp_con)) THEN
     DEALLOCATE(Spp_con)
  ENDIF
    IF (ALLOCATED(Spp_lub)) THEN
     DEALLOCATE(Spp_lub)
  ENDIF
    IF (ALLOCATED(Spp_rep)) THEN
     DEALLOCATE(Spp_rep)
  ENDIF
IF (ALLOCATED(eCrossOmega_ij)) THEN
     DEALLOCATE(eCrossOmega_ij)
  ENDIF
 IF (ALLOCATED(Omega_ij)) THEN
     DEALLOCATE(Omega_ij)
  ENDIF
  IF (ALLOCATED(eCrossOmegaij)) THEN
     DEALLOCATE(eCrossOmegaij)
  ENDIF
 IF (ALLOCATED(Omegaij)) THEN
     DEALLOCATE(Omegaij)
  ENDIF

END SUBROUTINE stress
