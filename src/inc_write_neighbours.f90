!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Sagaya Savarimuthu Prasanna Kumar, from Marco Ellero's group.
! - Jose Antonio Ruiz-Lopez, from Marco Ellero's group.
!    Currently from the Department of Applied Physics at
!    Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Sagaya Savarimuthu Prasanna Kumar.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE write_neighbours(this, step)
!---------------------------------------------
  ! Subroutine to write in the shell the 
  ! of particle part neighbours
  !-------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)            :: step
  INTEGER :: I, J
  INTEGER :: sumNeigh, sumCont
  REAL    :: avgNeigh, avgCont, frCont, Ovpc
  
  sumNeigh = 0
  avgNeigh = 0.0
  sumCont = 0
  avgCont = 0.0
  frCont = 0.0

  DO I = 1, this%N
     sumNeigh = sumNeigh + this%part(I)%N_neigh
     sumCont = sumCont + this%part(I)%N_cont

	IF (this%part(I)%N_cont > 0) then
	frCont = frCont + 1.0
	END IF
     
  ENDDO
  avgNeigh = sumNeigh*1.0/this%N
  avgCont = sumCont*1.0/this%N
  frCont = frCont*1.0/this%N
  Ovpc = this%Ovpc
  this%Ncont = frCont
  
 WRITE(this%output%AvgNeigh%unit, '(1I10, 5E20.10)')   &    
       step,                           &    !1
       avgNeigh,                       &    !2
       avgCont,                        &    !3
       frCont,                         &    !4	
       Ovpc				    !5
       
END SUBROUTINE write_neighbours
