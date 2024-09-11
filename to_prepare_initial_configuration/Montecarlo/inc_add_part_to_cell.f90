!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE add_part_to_cell(this, coord, part, error_out)
  !-----------------------------------------------
  ! This subroutine adds the particle Id to the 
  ! cell 'this'.
  !-----------------------------------------------
  use class_particle
  IMPLICIT NONE
  TYPE(cell_system_type), INTENT(inout) :: this
  INTEGER, DIMENSION(:), INTENT(in)     :: coord
  TYPE(particle_type), INTENT(inout)    :: part
  INTEGER, INTENT(out)                  :: error_out
  INTEGER :: I, J, K

  error_out = 0
 
  I = coord(1)
  J = coord(2)
  K = coord(3)

  this%coord(I,J,K)%N_part = this%coord(I,J,K)%N_part + 1
  IF (this%coord(I,J,K)%N_part > SIZE(this%coord(I,J,K)%list_part)) THEN
     error_out = 1
     WRITE(*,*) '*** add_part_to_cell error: the memory allocated for the '
     WRITE(*,*) '    list of particles of this cell is not enough. Please, '
     WRITE(*,*) '    allocate more memory to avoid this error. ***'
     GOTO 1000 !-- End of subroutine --
  ENDIF

  this%coord(I,J,K)%list_part(this%coord(I,J,K)%N_part) = part%Id
  part%cell0(:) = coord(:)

1000 CONTINUE

END SUBROUTINE add_part_to_cell
