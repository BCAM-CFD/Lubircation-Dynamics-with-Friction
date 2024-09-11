!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------- inc_calculate_Rij_sq --------------
!----------------------------------------------------
! Here we calculate the square distance of two particles.
! Requirements to add this file to a subroutine:
!    1.- You need to declare all the variables showed below
!    2.- You need to store the length of the box simulation in the Box variable
!    3.- Both particles should be labelled with the I and J integers.
!----------------------------------------------------

  Pos_ij(:) = this%part(I)%Pos(:) - this%part(J)%Pos(:)
  Pos_ij(:) = Pos_ij(:) - ANINT(Pos_ij(:)/this%Box(:))*this%Box(:)  !Periodic conditions
  Rij_sq    = DOT_PRODUCT(Pos_ij, Pos_ij)

!--------------- end inc_calculate_Rij_sq --------------
