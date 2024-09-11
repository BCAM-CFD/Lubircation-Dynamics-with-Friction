!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-----------------------------------------------------
SUBROUTINE change_cell_i(this, part, new_cell, old_cell, error_out)
!-----------------------------------------------------
  ! We change, if necessary, the list of particles of 
  ! the cells due to a movement of particle 'part'
  !---------------------------------------------------
  use class_particle
  IMPLICIT NONE
  TYPE(cell_system_type), INTENT(inout) :: this
  TYPE(particle_type), INTENT(inout)    :: part
  INTEGER, DIMENSION(:), INTENT(out)    :: new_cell
  INTEGER, DIMENSION(:), INTENT(in)     :: old_cell
  INTEGER, INTENT(out)                  :: error_out
  
  logical :: INCLUDED

  error_out = 0
  
  !-- First we calculate in what cell is i --
  CALL calculate_cell_i(this, part, new_cell, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Now we add the particle, if needed to the new cell, and delete
  !  it from the old one. --
  IF (.NOT.((new_cell(1) == old_cell(1)) .AND. &
       (new_cell(2) == old_cell(2)) .AND. &
       (new_cell(3) == old_cell(3)))) THEN

     CALL add_part_to_cell(this, new_cell, part, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of program --
     ENDIF

     CALL remove_part_from_cell(this%coord(old_cell(1), old_cell(2), old_cell(3)), part%Id, error_out) 

     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDIF

1000 CONTINUE

END SUBROUTINE change_cell_i
