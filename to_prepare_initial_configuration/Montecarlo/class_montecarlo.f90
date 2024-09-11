!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
 module class_montecarlo
   !--------------------------------
   ! Class of the whole sim
   !--------------------------------
   use class_computational
   IMPLICIT NONE

   TYPE montecarlo_type

      INTEGER  :: N_steps_check
      INTEGER  :: N_freq_stat
      REAL(Pr) :: amplitude
      REAL(Pr) :: min_amp
      REAL(Pr) :: max_amp
      REAL(Pr) :: acceptance_ratio
      REAL(Pr) :: acceptance
      REAL(Pr) :: beta
      
  END type montecarlo_type

  !-------- SUBROUTINES AND FUNCTIONS ---------------
CONTAINS
  
  include 'inc_read_montecarlo.f90'
  include 'inc_write_MC_info.f90'
  include 'inc_write_montecarlo.f90'
  include 'inc_initialize_montecarlo.f90'
  include 'inc_check_accept.f90'
  include 'inc_change_amp.f90'
  
end module class_montecarlo
