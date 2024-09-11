!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------------------------
SUBROUTINE compute_energy_i(this, cells, part, energy)
  !-----------------------------------------------------------------
  ! The energy that the presence of the particle 'part' gives to
  ! the system is computed.
  !-----------------------------------------------------------------
  use class_cell_system
  use class_potential
  IMPLICIT NONE
  TYPE(physics_type), INTENT(inout)  :: this
  TYPE(cell_system_type), INTENT(in) :: cells
  INTEGER, INTENT(In)                :: part
  REAL(Pr), INTENT(out)              :: energy
  INTEGER :: I, J, M, N, P, Q, R, S
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  INTEGER, DIMENSION(:), ALLOCATABLE  :: neigh_coord
  REAL(Pr) :: Rij
  REAL(Pr) :: Rij_sq

  ALLOCATE(pos_ij(this%dim))
  ALLOCATE(neigh_coord(this%dim))

  !--- The energy is summed up  ---
  energy = 0.0_Pr
  IF (this%dim == 2) THEN
     I = part
     M = this%part(part)%cell0(1)
     N = this%part(part)%cell0(2)
     DO R = 1, cells%coord(M,N,1)%N_neigh
        neigh_coord(1:this%dim) = cells%coord(M,N,1)%neigh(R)%coord(1:this%dim)
        DO S = 1, cells%coord(neigh_coord(1), neigh_coord(2), 1)%N_part
           J  = cells%coord(neigh_coord(1), neigh_coord(2), 1)%list_part(S)
           IF (I .NE. J) THEN
              include 'inc_calculate_Rij_sq.f90'
              IF (Rij_sq .LE. this%rcut_sq) THEN
                 !-- The energy is summed up only to I,
                 !   and in the end we multiply by 2 to take
                 !   into account the energy on J.
                 Rij = SQRT(Rij_sq)
                 energy = energy + potential(Rij)
                 
              ENDIF
           ENDIF
        ENDDO
     ENDDO
  ELSE !-- this%dim == 3 --
     I = part
     M = this%part(part)%cell0(1)
     N = this%part(part)%cell0(2)
     Q = this%part(part)%cell0(3)
     DO R = 1, cells%coord(M,N,Q)%N_neigh
        neigh_coord(1:this%dim) = &
             cells%coord(M,N,Q)%neigh(R)%coord(1:this%dim)
        DO S = 1, cells%coord(neigh_coord(1), neigh_coord(2), neigh_coord(3))%N_part
           J  = cells%coord(neigh_coord(1), neigh_coord(2), neigh_coord(3))%list_part(S)
           IF (I .NE. J) THEN
              include 'inc_calculate_Rij_sq.f90'
              
              IF (Rij_sq .LE. this%Rcut_sq) THEN
                 !-- The energy is summed up only to I,
                 !   and in the end we multiply by 2 to 
                 !   take into account the energy on J.
                 Rij = SQRT(Rij_sq)
                 
                 energy = energy + potential(Rij)
              ENDIF
           ENDIF
        ENDDO
     ENDDO
  ENDIF
!  energy = energy * 2.0_Pr

  DEALLOCATE(pos_ij)
  DEALLOCATE(neigh_coord)

END SUBROUTINE compute_energy_i


