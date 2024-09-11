!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------------------
SUBROUTINE read_pos(this, file, error_out)
!---------------------------------------------------------
  ! Subroutine to read the positions of the particles from 
  ! a file
  !-------------------------------------------------------
  use class_file
  use class_files_utilities
  IMPLICIT NONE
  TYPE(physics_type), INTENT(inout) :: this
  TYPE(file_type), INTENT(in)       :: file
  INTEGER, INTENT(out)              :: error_out
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: pos
  INTEGER :: I

  error_out = 0

  !-- The positions are storaged in the pos array --
  CALL file_to_array(file%name, pos, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
  
  this%N_part = SIZE(pos(:,1))
  ALLOCATE(this%part(this%N_part))
  DO I = 1, this%N_part
     CALL create_particle(this%part(I), this%dim, I)
     this%part(I)%pos(:) = pos(I,:)
  ENDDO
  
1000 CONTINUE

  IF (ALLOCATED(pos)) THEN
     DEALLOCATE(pos)
  ENDIF


END SUBROUTINE read_pos
