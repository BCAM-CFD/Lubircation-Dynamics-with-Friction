!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------------------------------------------------
SUBROUTINE check_part_cell(this, part, in)
  !--------------------------------------------------------
  ! Subroutine to check if a particle is in the list of particles
  ! of a cell.
  !------------------------------------------------------
  IMPLICIT NONE
  TYPE(cell_type), INTENT(in) :: this
  INTEGER, INTENT(in)         :: part
  LOGICAL, INTENT(out)        :: in
  INTEGER :: I

  in = .FALSE.
  DO I = 1, this%N_part
     IF (this%list_part(I) == part) THEN
        in = .TRUE.
        GOTO 100 !-- End of loop --
     ENDIF
  ENDDO
100 CONTINUE

END SUBROUTINE check_part_cell

