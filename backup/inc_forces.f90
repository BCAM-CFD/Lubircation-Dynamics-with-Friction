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
SUBROUTINE forces(this, error_out)
  !-----------------------------------
  ! Calculations of forces on particles (only repulsion forces).
  ! See Vazquez-Quesada et al, Journal of Non-Newtonian Fluid Mechanics, 2016.
  !-----------------------------------
  IMPLICIT NONE
  TYPE(system_type), intent(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I, J, K, L
  INTEGER :: Ncon, Ncontot
  REAL(Pr) :: Rij_sq
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box
  INTEGER :: dim
  REAL(Pr) :: Rij, ai, aj, am, ar
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Fcon, Frep
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vij, vt, U_p, U_en
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Omega_ij, eCrossOmega_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: defij, tij, eCrosstij
  REAL(Pr) :: F0, Fn, Ft
  REAL(Pr) :: eij_vij, eij_up, utan, def
  REAL(Pr) :: Som, mu, Ubar, Wbar, SRR, deltaij
  REAL(Pr) :: exp_taus
  REAL(Pr) :: sij, hij
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_forces.f90'  

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(Fcon(dim), Frep(dim))
  ALLOCATE(vij(dim), vt(dim), U_p(dim), U_en(dim))
  ALLOCATE(Omega_ij(dim),eCrossOmega_ij(dim))
  ALLOCATE(eCrosstij(dim))
  ALLOCATE(defij(dim), tij(dim))

  Box(:) = this%L(:)

  !-- Forces are put to zero --
  DO I = 1, this%N
     this%part(I)%force(:)         = 0.0_Pr
     this%part(I)%torque(:)        = 0.0_Pr
     this%wall%rep_force_top(:)    = 0.0_Pr
     this%wall%rep_force_bottom(:) = 0.0_Pr
  ENDDO

  !-- The forces are computed --
  this%Ovpc = 0.0_Pr
  this%defmax = 0.0_Pr
  DO I = 1, this%N

  Ncon=0
  ai= this%part(I)%R
 
     DO K = 1, this%part(I)%N_neigh
     
        J = this%part(I)%neigh_list(K)
        aj= this%part(J)%R
        am= (ai+aj)/2.0_Pr
        
        include 'inc_calculate_Rij_sq.f90'
        
           Fcon(:)= 0.0_Pr
           Frep(:)= 0.0_Pr

        IF (Rij_sq .LE. this%rcut_sq*am*am) THEN
           Rij = SQRT(Rij_sq) 
           eij(:) = pos_ij(:) / Rij
           ar=ai*aj/am
           sij = (Rij - ai - aj)/am
           hij = - sij
           deltaij = 2.0_Pr*this%rough/am
                   
           IF (sij < deltaij) THEN
           
             ! CALL error_header(file_name)              
             ! WRITE(*,*) '*** forces error: particles',I,'and', J,&
             !      'are penetrating one to each other. ***'
             ! error_out = 1
             ! GOTO 1000 !-- End of subroutine --


           exp_taus = EXP(-this%tau_rep * sij)
           exp_taus = min(exp_taus, 1.0_Pr)
           F0 = this%F0_rep * ar * exp_taus
           
           Frep(:) = F0 * eij(:)


             this%part(I)%cont_list(K)=1
             Ncon = Ncon + 1
	    
	     	     IF (hij > this%Ovpc) THEN
		     this%Ovpc = hij
		     ENDIF
		
			 
             vij(:)  = this%part(I)%vel(:) - this%part(J)%vel(:)
             eij_vij = DOT_PRODUCT(eij, vij)
             
             Fn = this%Fcn*(hij + deltaij)
             Fcon(:) = Fn*eij(:)
             
        Omega_ij(:)= (ai*this%part(I)%Omega(:))+ (aj*this%part(J)%Omega(:))                                             
        eCrossOmega_ij(1) = eij(2)*Omega_ij(3) - eij(3)*Omega_ij(2)
        eCrossOmega_ij(2) = eij(3)*Omega_ij(1) - eij(1)*Omega_ij(3)
        eCrossOmega_ij(3) = eij(1)*Omega_ij(2) - eij(2)*Omega_ij(1)
	         
                vt(:)=vij(:)+eCrossOmega_ij(:)-eij_vij*eij(:)
	         
                utan = DOT_PRODUCT(vt,vt)
                utan = sqrt(utan)
             
                defij(:)=this%part(I)%def_tan(K,:)+this%dt*vt(:)

	        def=DOT_PRODUCT(defij,defij)
	        def=sqrt(def)

	         IF (def/am > this%defmax) THEN
	         this%defmax = def/am
	         ENDIF

	         tij(:)=defij(:)/def

            eCrosstij(1) = eij(2)*tij(3) - eij(3)*tij(2)
            eCrosstij(2) = eij(3)*tij(1) - eij(1)*tij(3)
            eCrosstij(3) = eij(1)*tij(2) - eij(2)*tij(1)

            Ft=this%Fct*def
             
             ! -- Calcultate the friction coefficient --
             
       U_p(:) = 0.5_Pr*this%part(I)%vel(:) + 0.5_Pr*this%part(J)%vel(:)
       eij_up = DOT_PRODUCT(eij, U_p)
       
       U_en(:) = U_p(:) - eij_up * eij(:)
       
       Ubar = DOT_PRODUCT(U_en, U_en)
       Ubar = sqrt(Ubar)

       SRR = utan/Ubar
       Ubar=2.0_Pr*Ubar*this%eta0/this%Ep/ar
       
       Wbar = 4.0_Pr*(F0 + Fn)/this%Ep/ar/ar
      


             
             ! -- Calculate the friction coefficient --
             call calc_mu(this, Ubar, Wbar, SRR, mu)
             
             IF (Ft .LE. mu*Fn) THEN
                this%part(I)%def_tan(K,:)=defij(:)
                Fcon(:)= Fcon(:) - Ft * tij(:)
                this%part(I)%torque(:) = this%part(I)%torque(:) &
                + ai*Ft*eCrosstij(:)
                
             ELSE
             
                def = mu * Fn/ this%Fct
                this%part(I)%def_tan(K,:)= def * tij(:)
                Fcon(:) = Fcon(:) - mu * Fn * tij(:)
                this%part(I)%torque(:) = this%part(I)%torque(:) &
                + ai * mu * Fn * eCrosstij(:)
             
             ENDIF


           
           ELSE
           
           this%part(I)%cont_list(K)= 0
           this%part(I)%def_tan(K,:)= 0.0_Pr
           
           exp_taus = EXP(-this%tau_rep * sij)
           F0 = this%F0_rep * ar * exp_taus
           Fcon(:) = 0.0_Pr
           Frep(:) = F0 * eij(:)

           ENDIF
         
       this%part(I)%force(:) = this%part(I)%force(:) + Fcon(:) + Frep(:)
       this%part(J)%force(:) = this%part(J)%force(:) - Fcon(:) - Frep(:)

           
        ENDIF
     ENDDO

     this%part(I)%N_cont=Ncon
     
!-- Top wall --
     IF (this%part(I)%pos(dim) > this%L(dim) - this%rcut+2.0_Pr - ai) THEN
        Rij    = this%L(dim) - this%part(I)%pos(dim)
        eij(:) = 0.0_Pr
        eij(dim) = -1.0_Pr
        IF (Rij < ai) THEN
           CALL error_header(file_name)           
           WRITE(*,*) '*** forces error: top wall is being penetrated. ***'
           error_out = 1
           GOTO 1000 !-- End of subroutine --
        ENDIF
        
        sij = Rij - this%part(I)%R
        
     IF (sij < this%rcut_on * 0.00001_Pr)THEN
          sij = this%rcut_on * 0.00001_Pr
        ENDIF
        exp_taus = EXP(-this%tau_rep * sij/ai)
        F0 = this%F0_rep * this%tau_rep * exp_taus / &
             (1.0_Pr - exp_taus)
        Frep(:) = F0 * eij(:)
        
        this%part(I)%force(:)  = this%part(I)%force(:)  + Frep(:)
        this%wall%rep_force_top(:) = this%wall%rep_force_top(:) - Frep(:)
     ENDIF

     !-- Bottom wall --
     IF (this%part(I)%pos(dim) < ai+this%rcut-2.0_Pr) THEN
        Rij    = this%part(I)%pos(dim)
        eij(:) = 0.0_Pr
        eij(dim) = 1.0_Pr
        IF (Rij < ai) THEN
           CALL error_header(file_name)           
           WRITE(*,*) '*** forces error: bottom wall is being penetrated. ***'
           error_out = 1
           GOTO 1000 !-- End of subroutine --
        ENDIF
        
        sij = Rij- this%part(I)%R
        
      IF(sij <this%rcut_on * 0.00001_Pr )THEN
           sij = this%rcut_on * 0.00001_Pr
        ENDIF
        exp_taus = EXP(-this%tau_rep * sij/ai)
        F0 = this%F0_rep * this%tau_rep * exp_taus / &
             (1.0_Pr - exp_taus)
        Frep(:) = F0 * eij(:)
        this%part(I)%force(:)     = this%part(I)%force(:)     + Frep(:)
  this%wall%rep_force_bottom(:) = this%wall%rep_force_bottom(:) - Frep(:)
     ENDIF

  ENDDO

  ! --- Accelerations of the particles are calculated ---
  DO I = 1, this%N
     this%part(I)%acc(:) = this%part(I)%force(:) / this%part(I)%mass
  ENDDO

  !-- The total force on the walls is calculated --
  this%wall%force_top(:) = this%wall%rep_force_top(:) + &
       this%wall%lub_force_top(:)
  this%wall%force_bottom(:) = this%wall%rep_force_bottom(:) + &
       this%wall%lub_force_bottom(:)
       

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
  IF (ALLOCATED(Fcon)) THEN
     DEALLOCATE(Fcon)
  ENDIF
  
  IF (ALLOCATED(Frep)) THEN
     DEALLOCATE(Frep)
  ENDIF
  
  IF (ALLOCATED(vij)) THEN
     DEALLOCATE(vij)
  ENDIF
  
  IF (ALLOCATED(vt)) THEN
     DEALLOCATE(vt)
  ENDIF
  
  IF (ALLOCATED(U_p)) THEN
     DEALLOCATE(U_p)
  ENDIF

  IF (ALLOCATED(U_en)) THEN
     DEALLOCATE(U_en)
  ENDIF

  IF (ALLOCATED(tij)) THEN
     DEALLOCATE(tij)
  ENDIF
  
  IF (ALLOCATED(Omega_ij)) THEN
     DEALLOCATE(Omega_ij)
  ENDIF
  
  IF (ALLOCATED(eCrossOmega_ij)) THEN
     DEALLOCATE(eCrossOmega_ij)
  ENDIF
  
  IF (ALLOCATED(eCrosstij)) THEN
     DEALLOCATE(eCrosstij)
  ENDIF
  
  IF (ALLOCATED(defij)) THEN
     DEALLOCATE(defij)
  ENDIF
  
  
END SUBROUTINE forces
