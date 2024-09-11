!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------
SUBROUTINE check_compute_energy(this, cells, error_out)
!------------------------------------------------------------
  ! We check if the subroutines compute_energy and compute_energy_i
  ! are consistent one to each other.
  !----------------------------------------------------------
  use class_cell_system
  use class_potential
  IMPLICIT NONE
  TYPE(physics_type), INTENT(inout)  :: this
  TYPE(cell_system_type), INTENT(In) :: cells
  INTEGER, INTENT(out)               :: error_out
  REAL(Pr) :: total_energy2
  REAL(Pr) :: energy_partial
  INTEGER :: I
  
  error_out = 0
  
  CALL compute_energy(this, cells) 
  total_energy2 = 0.0_Pr
  DO I = 1, this%N_part
     CALL compute_energy_i(this, cells, I, energy_partial)
     total_energy2 = total_energy2 + energy_partial
  ENDDO

  IF (ABS(this%energy - total_energy2) > ABS(0.01_Pr * this%energy)) THEN
     error_out = 1
     WRITE(*,*) '*** check_compute_energy error: subroutine compute_energy '
     WRITE(*,*) '    and compute_energy_i are not consistent one to each other ***'
     WRITE(*,*) this%energy, total_energy2
     GOTO 1000 !-- End of subroutine --
  ENDIF
  
1000 CONTINUE  

END SUBROUTINE check_compute_energy
