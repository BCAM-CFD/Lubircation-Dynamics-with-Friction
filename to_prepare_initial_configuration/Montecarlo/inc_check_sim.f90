!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------- check_sim ----------

  IF (this%N_save .LE. 0) THEN
     WRITE(*,*) '*** Input error: N_save should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF (this%N_save_pos .LE. 0) THEN
     WRITE(*,*) '*** Input error: N_save_pos should be a positive number. ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  IF ((this%initial < 1) .OR. (this%initial > 2)) THEN
     WRITE(*,*) '*** Input error: option initial = ',this%initial,&
          ' is not available ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF


!---------------- END check_sim ----------
