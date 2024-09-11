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

!-------------------------------
SUBROUTINE PBC(this, error_out)
  !--------------------------
  ! This subroutine updates the positions of the particles if they go
  ! outside of the simulation box. The reference position pos0 to 
  ! know if the neighbours should be calculated is also updated.
  !--------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I, J
  INTEGER :: max_dim_PBC
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --

  error_out = 0

  file_name = 'inc_PBC.f90'

  max_dim_PBC = this%dim - 1
  
  !--- Fluid particles ---
  DO I = 1, this%N
     DO J = 1, max_dim_PBC
        IF (this%part(I)%pos(J) .GE. this%L(J)) THEN
           this%part(I)%pos(J)  = this%part(I)%pos(J) - this%L(J)
           this%part(I)%pos0(J) = this%part(I)%pos0(J) - this%L(J)
        ELSE IF (this%part(I)%pos(J) < 0) THEN
           this%part(I)%pos(J)  = this%part(I)%pos(J)  + this%L(J)
           this%part(I)%pos0(J) = this%part(I)%pos0(J) + this%L(J)
        ENDIF
     ENDDO
  ENDDO

  !-- Checking if the particles penetrate the walls --
!  DO I = 1, this%N
!     IF (this%part(I)%pos(this%dim) .GE. this%L(this%dim)) THEN
!        error_out = 1
!        CALL error_header(file_name)        
!        WRITE(*,*) '*** PBC error: top wall has been penetrated. ***'
!        GOTO 1000 !-- End of subroutine --
!     ENDIF
!     IF (this%part(I)%pos(J) < 0) THEN
!        error_out = 1
!        CALL error_header(file_name)        
!        WRITE(*,*) '*** PBC error: bottom wall has been penetrated. ***'
!        GOTO 1000 !-- End of subroutine --
!     ENDIF
!  ENDDO

1000 CONTINUE

END SUBROUTINE PBC
