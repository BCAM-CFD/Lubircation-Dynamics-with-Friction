!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------- check_physics --------
!-------------------------------------------------------
! We do some tests to be sure that the input 
! is correct.
!-------------------------------------------------------

  IF (this%dim .NE. SIZE(this%Box)) THEN
     WRITE(*,*) '*** Input error: dim and box are not consistent. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (this%N_part .LE. 0) THEN
     WRITE(*,*) '*** Input error: N_part should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF ((this%dim .NE. 2) .AND. (this%dim .NE. 3)) THEN
     WRITE(*,*) '*** Input error: dim should be 2 or 3. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (this%kT .LE. 0) THEN
     WRITE(*,*) '*** Input error: kT should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (this%rcut .LE. 0) THEN
     WRITE(*,*) '*** Input error: rcut should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

!------------ end check_physics --------
