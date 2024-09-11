!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE change_amp(this, N_accepted, N_tried)
  !-----------------------------------------------
  ! This subroutine decides according to the
  ! statistics of the last steps, if the 
  ! amplitude should be increased or decreased.
  ! After that, initializes the statistics
  ! variables.
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(montecarlo_type), INTENT(inout) :: this
  INTEGER, INTENT(inout)               :: N_accepted
  INTEGER, INTENT(inout)               :: N_tried
  INTEGER :: I
  
  !-- We check the amplitude of the MC method --
  this%acceptance = REAL(N_accepted, KIND = Pr) / REAL(N_tried, KIND = Pr)
  IF (this%acceptance < this%acceptance_ratio) THEN
     IF (this%amplitude * 0.95_Pr > this%min_amp) THEN
        this%amplitude = this%amplitude * 0.95_Pr
     ENDIF
  ELSE 
     IF (this%amplitude * 1.05_Pr < this%max_amp) THEN
        this%amplitude = this%amplitude * 1.05_Pr
     ENDIF
  ENDIF
  N_tried    = 0
  N_accepted = 0

!!$  WRITE(files%unit_output,*) 'Acceptance = ', system%acceptance
!!$  WRITE(files%unit_output,*) 'amplitude/rcut = '  , system%amplitude/system%rcut
!!$  WRITE(files%unit_output,*) 'amplitude  = '  , system%amplitude
!!$  WRITE(files%unit_output,*)

END SUBROUTINE change_amp
