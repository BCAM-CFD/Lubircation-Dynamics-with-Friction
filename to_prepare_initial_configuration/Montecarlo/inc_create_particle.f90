!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!------------------------------------------------------
  SUBROUTINE create_particle(this, dim, I)
!------------------------------------------------------
    ! Creator subroutine of a particle 
    !--------------------------------------------------
    IMPLICIT NONE
    TYPE(particle_type), INTENT(inout) :: this
    INTEGER, INTENT(in)                :: dim
    INTEGER, INTENT(in)                :: I
    
    ALLOCATE(this%pos(dim))
    ALLOCATE(this%old_pos(dim))

    this%Id = I
    
  END SUBROUTINE create_particle
