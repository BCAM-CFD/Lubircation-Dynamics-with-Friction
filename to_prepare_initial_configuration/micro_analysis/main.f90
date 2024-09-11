!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!----------------------------
PROGRAM micro
!----------------------------
  ! Program to study the microstructure
  ! of a cloud of points (min and max distance
  ! between particles for example).
  !--------------------------
  use class_files_utilities
  IMPLICIT NONE
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: pos
  CHARACTER(LEN=MAX_CHAR)               :: file
  INTEGER :: error_out
  REAL(Pr) :: min_dist
  INTEGER :: I, J
  REAL(Pr), dimension(:), ALLOCATABLE :: pos_ij
  REAL(Pr), dimension(:), ALLOCATABLE :: box
  REAL(Pr) :: Rij
  INTEGER :: dim
  INTEGER :: J_min

  error_out = 0

  !-------- inputs ---------------
  file = 'pos.dat'  
  dim = 3
  ALLOCATE(Box(dim))
  Box(1) = 32.0_Pr
  Box(2) = 32.0_Pr
  Box(3) = 1000.0_Pr
  !------------------------------
  
  ALLOCATE(pos_ij(dim))

  CALL file_to_array(file, pos, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of program --
  ENDIF
  !********* En pos estan guardadas todas las columnas del archivo *****

!!$  !************* Minimum distance ******************
  IF (SIZE(pos(1,:)) .NE. dim) THEN
     WRITE(*,*) '*** Main error: the number of columns of the file'
     WRITE(*,*) '    should be equal to dim. ***'
     error_out = 1
     GOTO 1000 !-- End of program --
  ENDIF
  min_dist = 20.0_Pr
  DO I = 1, SIZE(pos(:,1))
     DO J = I + 1, SIZE(pos(:,1))
        Pos_ij(:) = Pos(I,:) - Pos(J,:)
        Pos_ij(:) = Pos_ij(:) - ANINT(Pos_ij(:)/Box(:))*Box(:)  !Periodic conditions
        Rij    = SQRT(DOT_PRODUCT(Pos_ij, Pos_ij))
        IF (Rij < min_dist) THEN
           min_dist = Rij
        ENDIF
     ENDDO
  ENDDO
  WRITE(*,*) '*** Minimum distance = ', min_dist
!!$  !********** END Minimum distance ******************

  !************* Minimum distance per particle******************
!!$  DO I = 1, SIZE(pos(:,1))
!!$     min_dist = 20.0_Pr
!!$!     J_min = 1
!!$     DO J = 1, SIZE(pos(:,1))
!!$        IF (I .NE. J) THEN
!!$           Pos_ij(1:dim) = Pos(I,1:dim) - Pos(J,1:dim)
!!$           Pos_ij(1:dim) = Pos_ij(1:dim) - ANINT(Pos_ij(1:dim)/Box(1:dim))*Box(1:dim)  !Periodic conditions
!!$           Rij    = SQRT(DOT_PRODUCT(Pos_ij, Pos_ij))
!!$           IF (Rij < min_dist) THEN
!!$              min_dist = Rij
!!$!              J_min = J
!!$           ENDIF
!!$        ENDIF
!!$     ENDDO
!!$!     IF (min_dist < 2) THEN
!!$        WRITE(*,*) I, min_dist
!!$!        WRITE(*,*) 'AAA ',I, min_dist -2.0_Pr
!!$!     ENDIF
!!$  ENDDO
  !********** END Minimum distance per particle ******************

1000 CONTINUE

  IF (ALLOCATED(pos)) THEN
     DEALLOCATE(pos)
  ENDIF
  IF (ALLOCATED(pos_ij)) THEN
     DEALLOCATE(pos_ij)
  ENDIF
  IF (ALLOCATED(box)) THEN
     DEALLOCATE(box)
  ENDIF


END PROGRAM micro
