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
!--------------------------------------------
SUBROUTINE force_plane(this, coord, plane, force, error_out)
  !------------------------------------------
  ! The force between particles through a plane is computed
  ! in order to calculate the normal stress differences.
  ! This is unchecked and I am not sure if it works fine.
  ! coord 1 -> x
  ! coord 2 -> y
  ! coord 3 -> z
  ! plane is the value of the coordinate of the plane. For
  ! example, if coord = 1, and plane = 15.0, we are calculating the
  ! forces through the plane x = 15.0.
  !************************************************************
  ! For the moment I am not using this. Although 
  ! some components of the stress tensor match fine with the
  ! stress tensor obtained by Irving-Kirkwood, others are completely
  ! different (I do not know why)
  !************************************************************
  !------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)    :: this
  INTEGER, INTENT(in)                 :: coord
  REAL(Pr), INTENT(in)                :: plane
  REAL(Pr), DIMENSION(:), INTENT(out) :: force
  INTEGER, INTENT(out)                :: error_out
  REAL(Pr) :: R
  REAL(Pr) :: fij1
  REAL(Pr) :: fij2
  REAL(Pr) :: gij
  INTEGER  :: dim
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Fij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box
  REAL(Pr) :: Rij_sq
  REAL(Pr) :: Rij
  REAL(Pr) :: eij_vij
  REAL(Pr) :: s
  REAL(Pr) :: exp_taus
  REAL(Pr) :: F0
  INTEGER :: I, J, K
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_force_plane.f90'  

  !-- Eqs (5) from JNNFM (for equal radius) --
  R = this%part(1)%R
  fij1 = -6.0_Pr * this%pi * this%eta0 * R**2.0_Pr / 4.0_Pr
  fij2 = -6.0_Pr * this%pi * this%eta0 * R * 9.0_Pr / 40.0_Pr
  gij   = -6.0_Pr * this%pi * this%eta0 * R / 6.0_Pr

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(vij(dim))
  ALLOCATE(Fij(dim))

  Box(:) = this%L(:)

  force(:) = 0.0_Pr

  !-- Force is calculated --
  DO I = 1, this%N
     DO K = 1, this%part(I)%N_neigh
        J = this%part(I)%neigh_list(K)
        IF (((this%part(I)%pos(coord) < plane) .AND. &
             (this%part(J)%pos(coord) > plane)) &
             .OR. ((this%part(I)%pos(coord) > plane) .AND. &
             (this%part(J)%pos(coord) < plane))) THEN
           include 'inc_calculate_Rij_sq.f90'

           IF (Rij_sq .LE. this%rcut_sq) THEN
              Rij = SQRT(Rij_sq) 
              eij(:) = pos_ij(:) / Rij
              IF (Rij < this%rcut_on) THEN
                 Rij = this%rcut_on
              ENDIF
              vij(:) = this%part(I)%vel(:) - this%part(J)%vel(:)
              eij_vij = DOT_PRODUCT(eij, vij)

              s = Rij - this%part(I)%R - this%part(J)%R
              IF (s .LE. 0) THEN
                 CALL error_header(file_name)                 
                 WRITE(*,*) '*** force_plane error: &
                      particles',I,'and', J,&
                      'are penetrating one to each other. ***'
                 error_out = 1
                 GOTO 1000 !-- End of subroutine --
              ENDIF
              exp_taus = EXP(-this%tau_rep * s)
              F0 = this%F0_rep * this%tau_rep * exp_taus / &
                   (1.0_Pr - exp_taus)
              Fij(:) = F0 * eij(:)
              !-- Lubrication force --
              Fij(:) = Fij(:) + (fij1/s + &
                   fij2 * LOG(this%part(I)%R / s)) * &
                   eij_vij * eij(:) + &
                   gij * LOG(this%part(I)%R / s) * &
                   (vij(:) - eij_vij * eij(:))

              IF (this%part(I)%pos(coord) < plane) THEN
                 force(:) = force(:) - Fij(:)
              ELSE
                 force(:) = force(:) + Fij(:)
              ENDIF
           ENDIF
        ENDIF

     ENDDO
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
  IF (ALLOCATED(Fij)) THEN
     DEALLOCATE(Fij)
  ENDIF

END SUBROUTINE force_plane

