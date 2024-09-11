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

!--------------------------------------------------------
SUBROUTINE initial_positions(this, N, R, error_out)
!---------------------------------------------------------
  ! Initial positions and radii are assigned to particles.
  !-------------------------------------------------------
  use class_files_utilities
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)    :: this
  INTEGER, INTENT(in)                 :: N
  REAL(Pr), INTENT(in)                :: R
  INTEGER, INTENT(out)                :: error_out
  LOGICAL :: control_end
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: random
  INTEGER :: I, J, counter
  INTEGER :: max_counter
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: pos
  REAL(Pr) :: rij
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_initial_positions.f90'  

  !-- The maximum number of neighbours --
  ! If we consider hard spheres, unless long range potentials
  ! are considered, the maximum number of neighbours can not be 
  ! very high. I would say that no more than the number of vertices
  ! of an icosahedron. Just to be sure it is set to 30.
  this%Nmax_list = 40

  IF (this%read_pos) THEN
     !-------- Particles are read from a file ----------
     CALL file_to_array(this%file_pos, pos, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF

     IF (this%dim .NE. SIZE(pos,2)) THEN
     CALL error_header(file_name)        
        WRITE(*,*) '*** initial positions error: the number of columns from'
        WRITE(*,*) '    the particles file does not match with the number of dimensions. ***'
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
     this%N = SIZE(pos,1)
     IF (this%N <= 0) THEN
     CALL error_header(file_name)        
        WRITE(*,*) '*** initial positions error: Wrong number of particles ***'
        WRITE(*,*) 'N = ',this%N
        GOTO 1000 !-- End of subroutine --
     ENDIF
     ALLOCATE(this%part(this%N))
     DO I = 1, this%N
        CALL particle_constructor(this%part(I), this%dim, this%Nmax_list, error_out)
        IF (error_out .NE. 0) THEN
           GOTO 1000 !-- End of subroutine --
        ENDIF
        this%part(I)%pos(:) = pos(I,:)
     ENDDO     
     CALL initial_radius(this, R)
  ELSE 
     !---------- Particles are collocated randomly ----------------
     this%N = N
     ALLOCATE(this%part(this%N))
     DO I = 1, this%N
        CALL particle_constructor(this%part(I), this%dim, this%Nmax_list, error_out)
        IF (error_out .NE. 0) THEN
           GOTO 1000 !-- End of subroutine --
        ENDIF
     ENDDO
     CALL initial_radius(this, R)

     ALLOCATE(random(this%dim))
     ALLOCATE(pos_ij(this%dim))
     
     DO I = 1, this%N
        control_end = .FALSE.
        counter = 0
        max_counter = 10000
        DO WHILE(.NOT.(control_end))
           counter = counter + 1
           CALL RANDOM_NUMBER(random)
           random(:) = random(:) * this%L(:)
           
           IF (this%part(I)%R .LE.0) THEN
              error_out = 1
              CALL error_header(file_name)              
              WRITE(*,*) '*** Initial positions error: R should be a &
                   positive number. ***'
              GOTO 1000 !-- End of subroutine --
           ENDIF
           
           !-- We have walls in z = 0 and z = Lz --
           control_end = .TRUE.
           IF ((random(this%dim) < this%part(I)%R) .OR. &
                (random(this%dim) > this%L(this%dim) - this%part(I)%R)) THEN
              control_end = .FALSE.
              GOTO 100 !-- End of loop --
           ENDIF
           
           !-- Now we check if there is not overlapping with another
           !   particles --
           IF (control_end) THEN
              DO J = 1, I - 1
                 pos_ij(:) = random(:) - this%part(J)%pos(:)
                 !Periodic boundary conditions
                 Pos_ij(:) = Pos_ij(:) - &
                      ANINT(Pos_ij(:)/this%L(:))*this%L(:)  
                 rij = SQRT(DOT_PRODUCT(pos_ij, pos_ij))
                 IF (rij < (this%part(I)%R + this%part(J)%R)) THEN
                    control_end = .FALSE.
                    GOTO 10 !-- End of loop --
                 ENDIF
10            ENDDO
           ENDIF
           
           IF (control_end) THEN
              this%part(I)%pos(:) = random(:)
           ENDIF
           
           !-- If we have tried too much --
           IF (counter > max_counter) THEN
              CALL error_header(file_name)              
              error_out = 1
              WRITE(*,*) '*** Initial positions error: &
                   it was not possible to find an initial configuration. ***'
              GOTO 1000 !-- End of subroutine ---
           ENDIF
100     ENDDO
     ENDDO
  ENDIF
  
  1000 CONTINUE

  IF (ALLOCATED(random)) THEN
     DEALLOCATE(random)
  ENDIF
  IF (ALLOCATED(pos_ij)) THEN
     DEALLOCATE(pos_ij)
  ENDIF
  IF (ALLOCATED(pos)) THEN
     DEALLOCATE(pos)
  ENDIF
  

END SUBROUTINE initial_positions
