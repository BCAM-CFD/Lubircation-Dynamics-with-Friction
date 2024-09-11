!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------------------
SUBROUTINE write_all_files_info(this, file)
  !------------------------------------------------------------
  IMPLICIT NONE
  TYPE(sim_type), INTENT(in)  :: this
  TYPE(file_type), INTENT(in) :: file
  INTEGER :: I
  
  IF (.NOT.(ALLOCATED(this%file))) THEN
     OPEN(file%unit, FILE=trim(file%name), ACCESS='APPEND')
     WRITE(file%unit,*) '*** The files array is not allocated. ***'
     CLOSE(file%unit)
  ELSE
     DO I = 1, SIZE(this%file)
        CALL write_file_info(this%file(I), file)
     ENDDO
  ENDIF

END SUBROUTINE write_all_files_info
