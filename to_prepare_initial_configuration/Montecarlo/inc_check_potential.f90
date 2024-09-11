!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------- check_potential --------
!-------------------------------------------------------
! We do some tests to be sure that the input 
! is correct.
!-------------------------------------------------------


  IF (this%Ntable .LE. 0) THEN
     WRITE(*,*) '*** Input error: Ntable should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

!------------ end check_potential --------
