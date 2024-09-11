!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-----------------------------------------------------
SUBROUTINE write_pos(this, file, step)
  !-----------------------------------------------------
  ! Data about SDPD particles is written.
  !-------------------------------------------------
  use class_file
  IMPLICIT NONE
  TYPE(physics_type), INTENT(in) :: this
  TYPE(file_type), INTENT(inout) :: file
  INTEGER, INTENT(in)            :: step
  INTEGER                 :: dim
  CHARACTER(LEN=MAX_CHAR) :: formatting
  INTEGER                 :: I

  dim = SIZE(this%box)

  CALL update_name(file, step)

  !--- The formatting is calculated ---
  IF (dim == 2) THEN
     formatting = '(I8,2E20.10)'
  ELSE 
     formatting = '(I8,3E20.10)'
  ENDIF
  
  OPEN(file%unit, FILE=trim(file%name), FORM='FORMATTED', STATUS='UNKNOWN')

  DO I = 1, this%N_part
     WRITE(file%unit, formatting)           &    !2D    !3D
          I,                                &    !1     !1 
          this%part(I)%pos                       !2,3   !2,3,4
  ENDDO

  CLOSE(file%unit)

END SUBROUTINE write_pos
