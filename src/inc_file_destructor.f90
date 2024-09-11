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
!---------------------------------
  SUBROUTINE file_destructor(this)
    !------------------------------------
    ! Destructor of class_file.
    !------------------------------------
    use class_files_utilities
    IMPLICIT NONE
    TYPE(file_type), INTENT(inout)       :: this
    LOGICAL :: opened

    !-- We check if its unit is still opened or not --
    INQUIRE(unit=this%unit, opened = opened) 
    IF (opened) THEN
       CLOSE(this%unit)
    ENDIF
    
  END SUBROUTINE file_destructor
