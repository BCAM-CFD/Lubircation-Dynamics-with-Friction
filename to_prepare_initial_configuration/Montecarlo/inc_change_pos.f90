!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE change_pos(this, part, amplitude)
  !-----------------------------------------------
  ! This subroutine moves the particle 'part'
  ! to another position, randomly.
  !-----------------------------------------------
  use class_computational
  IMPLICIT NONE
  TYPE(physics_type), INTENT(inout) :: this
  INTEGER, INTENT(in)               :: part
  REAL(Pr), INTENT(in)              :: amplitude
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: random_vector
  INTEGER :: I

  ALLOCATE(random_vector(this%dim))

  CALL random_number(random_vector)
  random_vector(:) = 2.0_Pr * random_vector(:) - 1.0_Pr
  this%part(part)%pos(:) = this%part(part)%pos(:) + &
       amplitude * random_vector(:)
  !-- Periodic boundary conditions --
  DO I = 1, this%dim
     IF ((this%part(part)%pos(I)) .GE. this%box(I)) THEN
        this%part(part)%pos(I)  = &
             this%part(part)%pos(I) - this%box(I)
     ELSE IF ((this%part(part)%pos(I)) < 0) THEN
        this%part(part)%pos(I)  = this%part(part)%pos(I) + &
             this%box(I)
     ENDIF
  ENDDO

  DEALLOCATE(random_vector)

END SUBROUTINE change_pos
