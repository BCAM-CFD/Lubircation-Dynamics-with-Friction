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

!-------------------------------------------------------
SUBROUTINE write_info(this, error_out)
  !----------------------------------------------------
  ! The info about the simulation is written out in 
  ! the file info.dat
  !----------------------------------------------------
  use class_files_utilities
  IMPLICIT NONE
  TYPE(system_type), INTENT(in) :: this
  INTEGER, INTENT(out)          :: error_out
  INTEGER                       :: unit
  CHARACTER(LEN=MAX_CHAR) :: name
  CHARACTER(LEN=MAX_CHAR) :: form    
  CHARACTER(LEN=MAX_CHAR) :: form_int

  error_out = 0

  name = this%output%info%name

  !-- We look for a free unit number --
  CALL search_unit(unit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The system info is written --
  OPEN(unit, FILE=trim(name), FORM='FORMATTED', STATUS='UNKNOWN')
  WRITE(unit,*) '************** SIMULATION INFO ***************'
  WRITE(unit,*)
  WRITE(unit,*) '------ System Object -------'
  WRITE(unit,*) 'dim       = ', this%dim
  IF (this%dim == 2) THEN
     form = '(A, 2F10.5)'
     form_int = '(A, 2I5)'
  ELSE
     form = '(A, 3F10.5)'
     form_int = '(A, 3I5)'
  ENDIF
  WRITE(unit,form) ' L          = ', this%L(1:this%dim)
  WRITE(unit,*) 'gamma_dot  = ',this%gamma_dot
  WRITE(unit,*) 'N          = ', this%N
  WRITE(unit,*) 'Nsteps     = ', this%Nsteps
  WRITE(unit,*) 'dt         = ', this%dt
  WRITE(unit,*) 'dt_sq      = ', this%dt_sq
  WRITE(unit,*) 'rcut       = ', this%rcut
  WRITE(unit,*) 'rcut_sq    = ', this%rcut_sq
  WRITE(unit,*) 'rcut_on    = ', this%rcut_on
  WRITE(unit,*) 'rcut_on_sq = ', this%rcut_on_sq
  WRITE(unit,*) 'rlist      = ', this%rlist
  WRITE(unit,*) 'rmax       = ', this%rmax
  WRITE(unit,*) 'rmax_sq    = ', this%rmax_sq
  WRITE(unit,*) 'pi         = ', this%pi
  WRITE(unit,*) 'read_pos   = ', this%read_pos
  WRITE(unit,*) 'file_pos   = ', trim(this%file_pos)
  WRITE(unit,*) 'read_vel   = ', this%read_vel
  WRITE(unit,*) 'file_vel   = ', trim(this%file_vel)
  WRITE(unit,*) 'read_Omega   = ', this%read_Omega
  WRITE(unit,*) 'file_Omega   = ', trim(this%file_Omega)
  WRITE(unit,*) 'eta0       = ', this%eta0
  WRITE(unit,*) 'eta1       = ', this%eta1
  WRITE(unit,*) 'R_par       = ', this%R_par
  WRITE(unit,*) 'a_par       = ', this%a_par
  WRITE(unit,*) 'gamma_dot_critical       = ', this%gamma_dot_critical
  WRITE(unit,*) 'V           = ', this%V  
  WRITE(unit,*) 'sweep_tol  = ', this%sweep_tol
  WRITE(unit,*) 'explicit   = ', this%explicit
  WRITE(unit,*) 'd_bulk     = ', this%d_bulk
  WRITE(unit,*) 'L_bulk     = ', this%L_bulk

  !-- The neighbours info is written --
  WRITE(unit,*)   
  WRITE(unit,*) '--------- neighbours info --------------'
  WRITE(unit,*) 'Npart_max = ', this%Npart_max
  WRITE(unit,*) 'Nmax_list = ', this%Nmax_list
  WRITE(unit,form_int) ' Ncells    = ', this%Ncells(:)

  !-- The repulsion force info is written --
  WRITE(unit,*)   
  WRITE(unit,*) '--------- Repulsion force info --------------'
  WRITE(unit,*) 'tau = ', this%tau_rep
  WRITE(unit,*) 'F0  = ', this%F0_rep

  CLOSE(unit)

  !-- The wall info is written --
  CALL write_wall_info(this%wall, this%output%info, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The random info is written --
  CALL write_random_info(this%random, this%output%info, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The output info is written --
  CALL write_output_info(this%output, this%output%info, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  WRITE(*,*) '*** Info written ***'

1000 CONTINUE  

END SUBROUTINE write_info
