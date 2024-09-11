!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-----------------------------------------------------
SUBROUTINE write_potential_table(this, file)
  !-----------------------------------------------------
  ! Particles positions are written.
  !-------------------------------------------------
  use class_file
  IMPLICIT NONE
  TYPE(potential_type), INTENT(In) :: this
  TYPE(file_type), INTENT(in)      :: file
  REAL(Pr) :: x
  INTEGER  :: I
  
  OPEN(file%unit, FILE=trim(file%name), FORM='FORMATTED', &
       STATUS='UNKNOWN')

  DO I = 1, this%Ntable
     x = REAL(I, KIND = Pr) / this%aux 
     WRITE(file%unit,*) x   , &
          potential_table(this, x), &
          potential(x)
  ENDDO

  CLOSE(file%unit)

END SUBROUTINE write_potential_table
