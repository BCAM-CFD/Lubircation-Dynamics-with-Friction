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

!-------------------------------------------------
SUBROUTINE cell_constructor(this, dim, Npart_max, limit_wall, error_out)
  !-------------------------------------------------
  ! Constructor of the class cell
  !-------------------------------------------------
  IMPLICIT NONE
  TYPE(cell_type), INTENT(inout) :: this
  INTEGER, INTENT(in)            :: dim
  INTEGER, INTENT(in)            :: Npart_max
  LOGICAL, INTENT(in)            :: limit_wall
  INTEGER, INTENT(out)           :: error_out
  INTEGER :: N_neigh_max
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0


  file_name = 'inc_cell_constructor.f90'  
  
  ALLOCATE(this%min_coord(dim))
  ALLOCATE(this%max_coord(dim))
  ALLOCATE(this%list_part(Npart_max))
  
  IF (dim == 2) THEN !every cell is neighbour of itself
     IF (.NOT.(limit_wall)) THEN
        this%N_neigh = 9
     ELSE
        this%N_neigh = 6
     ENDIF
  ELSE IF (dim == 3) THEN
     IF (.NOT.(limit_wall)) THEN
        this%N_neigh = 27
     ELSE
        this%N_neigh = 18
     ENDIF
  ELSE
     CALL error_header(file_name)     
     WRITE(*,*) '*** Cell constructor error: dim should be 2 or 3 ***'
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The memory for the neighbours of every cell is allocated --
  ALLOCATE(this%neigh_coord(this%N_neigh, dim))
  
1000 CONTINUE

END SUBROUTINE cell_constructor
