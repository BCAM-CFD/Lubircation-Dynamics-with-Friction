!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!   Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Jose Antonio Ruiz Lopez, currently from the Department of applied physics at
!   Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!   in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------

!---------------------------------------------
SUBROUTINE Calculate_cells_list(this, error_out)
  !-----------------------------------------------
  ! This subroutine calculates the list of particles
  ! for every cell.
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)   :: this
  INTEGER, INTENT(out)               :: error_out
  INTEGER      :: I, J, K, L
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  
  
  error_out = 0

  file_name = 'inc_calculate_cells_list.f90'  

  this%cell(:,:,:)%Npart = 0

  IF (this%dim == 2) THEN
     DO L = 1, this%N
        DO I = 1, this%Ncells(1)
           !--- If it is inside the x range ---
              IF ((this%part(L)%pos(1) .GE. this%cell(I,J,1)%min_coord(1))  .AND. &
                   (this%part(L)%pos(1) < this%cell(I,J,1)%max_coord(1))) THEN
                 DO J = 1, this%Ncells(2)
                    !--- and into the y range ---
                    IF ((this%part(L)%pos(2) .GE. this%cell(I,J,1)%min_coord(2)) .AND. &
                         (this%part(L)%pos(2) < this%cell(I,J,1)%max_coord(2))) THEN
                       this%cell(I,J,1)%Npart = this%cell(I,J,1)%Npart + 1
                       IF (this%cell(I,J,1)%Npart > this%Npart_max) THEN
                          error_out = 1
                          CALL error_header(file_name)                          
                          WRITE(*,*) '*** Calculate cells list error: Npart_max too small. ***'
                          GOTO 1000 !-- End of subroutine --
                       ENDIF
                       this%cell(I,J,1)%list_part(this%cell(I,J,1)%Npart) = L
                       GOTO 997
                    ENDIF
                 ENDDO
              ENDIF
           ENDDO
997     CONTINUE        
     ENDDO
  ELSE !--dim == 3 --
     DO L = 1, this%N
        DO I = 1, this%Ncells(1)
           !--- If it is inside the x range ---
           IF ((this%part(L)%pos(1) .GE. this%cell(I,1,1)%min_coord(1)) .AND. &
                (this%part(L)%pos(1) < this%cell(I,1,1)%max_coord(1))) THEN
              DO J = 1, this%Ncells(2)
                 !--- and into the y range ---
                 IF ((this%part(L)%pos(2) .GE. this%cell(1,J,1)%min_coord(2)) .AND. &
                      (this%part(L)%pos(2) < this%cell(1,J,1)%max_coord(2))) THEN
                    DO K = 1, this%Ncells(3)
                       !--- and into the z range ---
                       IF ((this%part(L)%pos(3) .GE. this%cell(1,1,K)%min_coord(3)) .AND.&
                            (this%part(L)%pos(3) < this%cell(1,1,K)%max_coord(3))) THEN
                          this%cell(I,J,K)%Npart = this%cell(I,J,K)%Npart + 1
                          IF (this%cell(I,J,K)%Npart > this%Npart_max) THEN
                             error_out = 1
                             CALL error_header(file_name)                             
                             WRITE(*,*) '*** Calculate cells list error: Npart_max too small. ***'
                             GOTO 1000 !-- End of subroutine --
                          ENDIF
                          this%cell(I,J,K)%list_part(this%cell(I,J,K)%Npart) = L
                          GOTO 998
                       ENDIF
                    ENDDO
                 ENDIF
              ENDDO
           ENDIF
        ENDDO
998  CONTINUE        
     ENDDO
  ENDIF

1000 CONTINUE
              
END SUBROUTINE Calculate_cells_list
