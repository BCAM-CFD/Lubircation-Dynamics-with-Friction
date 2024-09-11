!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------------
SUBROUTINE initialize_positions(this)
!------------------------------------------------------------
  IMPLICIT NONE
  TYPE(physics_type), INTENT(inout) :: this
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: ran
  INTEGER :: I

  ALLOCATE(this%part(this%N_part))
  DO I = 1, this%N_part
     CALL create_particle(this%part(I), this%dim, I)
  ENDDO

  ALLOCATE(ran(this%dim))

  DO I = 1, SIZE(this%part)
     CALL random_number(ran)
     this%part(I)%pos(:) = ran(:) * this%box(:)
     this%part(I)%old_pos(:) = this%part(I)%pos(:)
  ENDDO

  DEALLOCATE(ran)

END SUBROUTINE initialize_positions
