!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE remove_part_from_cell(this, Id, error_out)
  !-----------------------------------------------
  ! This subroutine removes the particle 'Id' from
  ! the list of particles of cell 'this'
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(cell_type), INTENT(inout) :: this
  INTEGER, INTENT(in)            :: Id
  INTEGER, INTENT(out)           :: error_out
  INTEGER :: I
  LOGICAL :: control

  error_out = 0

  control = .FALSE.
  DO I = 1, this%N_part
     IF (control) THEN
        this%list_part(I-1) = this%list_part(I)
     ENDIF
     IF (this%list_part(I) == Id) THEN
        control = .TRUE.
     ENDIF
  ENDDO

  this%N_part = this%N_part - 1

  IF (.NOT.(control)) THEN
     error_out = 1
     WRITE(*,*) '*** Remove_part_from_cell error: the particle was '
     WRITE(*,*) '    not found into the cell ***'
     WRITE(*,*) Id
     GOTO 1000 !-- End of subroutine --
  ENDIF

1000 CONTINUE

END SUBROUTINE remove_part_from_cell
