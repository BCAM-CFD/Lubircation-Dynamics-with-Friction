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

!------------------------------------------------
SUBROUTINE cells_edges(this, cell_length, L_min)
  !------------------------------------------------
  ! The edge positions of every cell are calculated
  !------------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)   :: this
  REAL(Pr), DIMENSION(:), INTENT(in) :: cell_length
  REAL(Pr), DIMENSION(:), INTENT(in) :: L_min
  INTEGER  :: I, J, K
  REAL(Pr) :: Lx0, Ly0, Lz0
  REAL(Pr) :: Lx , Ly , Lz

  IF (this%dim == 2) THEN
     Lx0 = L_min(1)
     DO I = 1, this%Ncells(1)
        Lx = Lx0 + cell_length(1)
        Ly0 = L_min(2)
        DO J = 1, this%Ncells(2)
           Ly = Ly0 + cell_length(2)
           this%cell(I,J,1)%min_coord(1) = Lx0
           this%cell(I,J,1)%max_coord(1) = Lx
           this%cell(I,J,1)%min_coord(2) = Ly0
           this%cell(I,J,1)%max_coord(2) = Ly
           Ly0 = Ly
        ENDDO
        Lx0 = Lx
     ENDDO
  ELSE !-- dim == 3 --
     Lx0 = L_min(1)
     DO I = 1, this%Ncells(1)
        Lx = Lx0 + cell_length(1)
        Ly0 = L_min(2)
        DO J = 1, this%Ncells(2)
           Ly = Ly0 + cell_length(2)
           Lz0 = L_min(3)
           DO K = 1, this%Ncells(3)
              Lz = Lz0 + cell_length(3)
              this%cell(I,J,K)%min_coord(1) = Lx0
              this%cell(I,J,K)%max_coord(1) = Lx
              this%cell(I,J,K)%min_coord(2) = Ly0
              this%cell(I,J,K)%max_coord(2) = Ly
              this%cell(I,J,K)%min_coord(3) = Lz0
              this%cell(I,J,K)%max_coord(3) = Lz
              Lz0 = Lz
           ENDDO
           Ly0 = Ly
        ENDDO
        Lx0 = Lx
     ENDDO
  ENDIF

END SUBROUTINE cells_edges
