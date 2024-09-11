!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-----------------------------------------------------
SUBROUTINE write_potential(this, file, N)
  !-----------------------------------------------------
  ! Particles positions are written.
  !-------------------------------------------------
  use class_file
  IMPLICIT NONE
  TYPE(potential_type), INTENT(in) :: this
  TYPE(file_type), INTENT(in)      :: file
  INTEGER, INTENT(in)              :: N
  INTEGER  :: I
  REAL(Pr) :: dx
  REAL(Pr) :: x
  
  dx = this%rcut / REAL(N, KIND = Pr)
  
  OPEN(file%unit, FILE=trim(file%name), FORM='FORMATTED', &
       STATUS='UNKNOWN')

  x = 0.5_Pr * dx
  DO I = 1, N
     WRITE(file%unit,*) x, &
          potential(X)
     x = x + dx
  ENDDO

  CLOSE(file%unit)

END SUBROUTINE write_potential
