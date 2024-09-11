!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!--------------------- number_checkings -----------------------

    SELECT CASE(checking)
       CASE (1) !variable should be positive
          IF (variable .LE. 0) THEN
             WRITE(*,*) 
             WRITE(*,*) '*** Input error : ',trim(this%variable_name), &
                  ' should be positive. ***'
             WRITE(*,*)
             error_out = -1
          ENDIF
       CASE (2) !variable should not be negative
          IF (variable < 0) THEN
             WRITE(*,*) 
             WRITE(*,*) '*** Input error : ',trim(this%variable_name), &
                  ' should not be negative. ***'
             WRITE(*,*)
             error_out = -2
          ENDIF

       END SELECT

!----------------- END number_checkings -----------------------
