!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------------------------
SUBROUTINE compute_energy(this, cells)
  !-----------------------------------------------------------------
  ! The energy of the whole system is calculated.
  !-----------------------------------------------------------------
  use class_cell_system
  use class_potential
  IMPLICIT NONE
  TYPE(physics_type), INTENT(inout)  :: this
  TYPE(cell_system_type), INTENT(in) :: cells
  INTEGER :: I, J, M, N, P, Q, R, S
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  INTEGER, DIMENSION(:), ALLOCATABLE  :: neigh_coord
  REAL(Pr) :: Rij
  REAL(Pr) :: Rij_sq

  ALLOCATE(pos_ij(this%dim))
  ALLOCATE(neigh_coord(this%dim))

  !--- The energy is summed up  ---
  this%energy = 0.0_Pr
  IF (this%dim == 2) THEN
     DO M = 1, cells%Ncells(1)
        DO N = 1, cells%Ncells(2)
           DO P = 1, cells%coord(M,N,1)%N_part
              I = cells%coord(M,N,1)%list_part(P)
              DO R = 1, cells%coord(M,N,1)%N_neigh
                 neigh_coord(1:this%dim) = cells%coord(M,N,1)%neigh(R)%coord(1:this%dim)
                 DO S = 1, cells%coord(neigh_coord(1), neigh_coord(2), 1)%N_part
                    J  = cells%coord(neigh_coord(1), neigh_coord(2), 1)%list_part(S)
                    IF (I < J) THEN
                       include 'inc_calculate_Rij_sq.f90'
                       IF (Rij_sq .LE. this%rcut_sq) THEN
                          !-- The energy is summed up only to I,
                          !   and in the end we multiply by 2 to take
                          !   into account the energy on J.
                          Rij = SQRT(Rij_sq)
                          this%energy = this%energy + potential(Rij)

                       ENDIF
                    ENDIF
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ELSE !-- this%dim == 3 --
     DO M = 1, cells%Ncells(1)
        DO N = 1, cells%Ncells(2)
           DO Q = 1, cells%Ncells(3)
              DO P = 1, cells%coord(M,N,Q)%N_part
                 I = cells%coord(M,N,Q)%list_part(P)
                 DO R = 1, cells%coord(M,N,Q)%N_neigh
                    neigh_coord(1:this%dim) = &
                         cells%coord(M,N,Q)%neigh(R)%coord(1:this%dim)
                    DO S = 1, cells%coord(neigh_coord(1), neigh_coord(2), neigh_coord(3))%N_part
                       J  = cells%coord(neigh_coord(1), neigh_coord(2), neigh_coord(3))%list_part(S)
                       IF (I < J) THEN
                          include 'inc_calculate_Rij_sq.f90'

                          IF (Rij_sq .LE. this%Rcut_sq) THEN
                             !-- The energy is summed up only to I,
                             !   and in the end we multiply by 2 to 
                             !   take into account the energy on J.
                             Rij = SQRT(Rij_sq)

                             this%energy =this%energy +potential(Rij)
                          ENDIF
                       ENDIF
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ENDIF
  this%energy = this%energy * 2.0_Pr !-- We take into account pairs IJ, but not JI --

  DEALLOCATE(pos_ij)
  DEALLOCATE(neigh_coord)

END SUBROUTINE compute_energy
