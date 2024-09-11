!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------------------------------
SUBROUTINE calculate_cells_list(this, part, error_out)
  !-------------------------------------------------------------------
  ! The list of particles in every cell is calculated.
  !-------------------------------------------------------------------
  use class_particle
  IMPLICIT NONE
  TYPE(cell_system_type), INTENT(inout)            :: this
  TYPE(particle_type), DIMENSION(:), INTENT(inout) :: part
  INTEGER, INTENT(out)                             :: error_out
  INTEGER :: dim
  INTEGER :: N_T
  LOGICAL, DIMENSION(:), ALLOCATABLE :: stored
  INTEGER :: I, J, K, L

  error_out = 0

  dim = SIZE(part(1)%pos)
  N_T = SIZE(part)
  
  ALLOCATE(stored(N_T))
  stored(:) = .FALSE. !-- For checking purposes --
  this%coord(:,:,:)%N_part = 0

  IF (dim == 2) THEN
     DO L = 1, N_T
        DO I = 1, this%Ncells(1)
           DO J = 1, this%Ncells(2)
              !--- If it is inside the x range ---
              IF ((part(L)%pos(1) .GE. this%coord(I,J,1)%L(1)%min)  &
                   .AND. (part(L)%pos(1) < this%coord(I,J,1)%L(1)%max)) THEN
                 !--- and into the y range ---
                 IF ((part(L)%pos(2) .GE. this%coord(I,J,1)%L(2)%min)  &
                      .AND. (part(L)%pos(2) < this%coord(I,J,1)%L(2)%max)) THEN
                    stored(L) = .TRUE.
                    this%coord(I,J,1)%N_part = this%coord(I,J,1)%N_part + 1
                    IF (this%coord(I,J,1)%N_part > SIZE(this%coord(I,J,1)%list_part)) THEN
                       WRITE(*,*) '** calculate cells list: there are more particles in one '
                       WRITE(*,*) '   cell than estimated. Please reserve more memory '
                       WRITE(*,*) '   for list_part in cell_system_constructor subroutine. ***'
                       error_out = 1
                       GOTO 1000 !-- End of subroutine --
                    ENDIF
                    this%coord(I,J,1)%list_part(this%coord(I,J,1)%N_part) = L
                    part(L)%cell0(1) = I
                    part(L)%cell0(2) = J
                    part(L)%cell0(3) = 1
                    GOTO 997
                 ENDIF
              ENDIF
           ENDDO
        ENDDO
        !-- If this line is reached, it means that particle L has not
        !   been stored in any cell --
        WRITE(*,*) '*** Calculate cells list error: the particle ',&
             L,' has not been stored in any cell. ***'
        WRITE(*,*) 'position = ', part(L)%pos
        error_out = 1
        GOTO 1000 !-- End of subroutine --
997     CONTINUE        
     ENDDO
  ELSE !--dim == 3 --
     DO L = 1, N_T
        DO I = 1, this%Ncells(1)
           DO J = 1, this%Ncells(2)
              DO K = 1, this%Ncells(3)
                 !--- If it is inside the x range ---
                 IF ((part(L)%pos(1) .GE. this%coord(I,J,K)%L(1)%min)  &
                      .AND. (part(L)%pos(1) < this%coord(I,J,K)%L(1)%max)) THEN
                    !--- and into the y range ---
                    IF ((part(L)%pos(2) .GE. this%coord(I,J,K)%L(2)%min)  &
                         .AND. (part(L)%pos(2) < this%coord(I,J,K)%L(2)%max)) THEN
                       !--- and into the z range ---
                       IF ((part(L)%pos(3) .GE. this%coord(I,J,K)%L(3)%min)  &
                            .AND. (part(L)%pos(3) < this%coord(I,J,K)%L(3)%max)) THEN
                          stored(L) = .TRUE.
                          this%coord(I,J,K)%N_part = this%coord(I,J,K)%N_part + 1
                          IF (this%coord(I,J,K)%N_part > SIZE(this%coord(I,J,K)%list_part)) THEN
                             WRITE(*,*) '** calculate cells list: there are more particles in one '
                             WRITE(*,*) '   cell than estimated. Please reserve more memory '
                             WRITE(*,*) '   for list_part in cell_system_constructor subroutine. ***'
                             WRITE(*,*) '   More than ', SIZE(this%coord(I,J,K)%list_part)
                             error_out = 1
                             GOTO 1000 !-- End of subroutine --
                          ENDIF
                          this%coord(I,J,K)%list_part(this%coord(I,J,K)%N_part) = L
                          part(L)%cell0(1) = I
                          part(L)%cell0(2) = J
                          part(L)%cell0(3) = K
                          GOTO 998
                       ENDIF
                    ENDIF
                 ENDIF
              ENDDO
           ENDDO
        ENDDO
        !-- If this line is reached, it means that particle L has not
        !   been stored in any cell --
        WRITE(*,*) '*** Calculate cells list error: the particle ',&
             L,' has not been stored in any cell. ***'
        WRITE(*,*) 'position = ', part(L)%pos
        error_out = 1
        GOTO 1000 !-- End of subroutine --
998     CONTINUE        
     ENDDO
  ENDIF

1000 CONTINUE

  IF (ALLOCATED(stored)) THEN
     DEALLOCATE(stored)
  ENDIF


END SUBROUTINE calculate_cells_list
