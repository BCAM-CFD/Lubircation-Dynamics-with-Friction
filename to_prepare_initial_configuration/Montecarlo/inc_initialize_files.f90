!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------------
  SUBROUTINE initialize_files(this, error_out)
 !--------------------------------------------------
    ! Subroutine to initialize files used during 
    ! simulation. An output directory is also created.
    ! If you need to add a file this is the
    ! place
    !-----------------------------------------------
    use class_files_utilities
    IMPLICIT NONE
    TYPE(sim_type), INTENT(inout) :: this
    INTEGER, INTENT(out)          :: error_out
    CHARACTER(LEN=MAX_CHAR), DIMENSION(:), ALLOCATABLE :: list_names
    INTEGER :: N_files
    INTEGER :: I 
    LOGICAL :: dir_log

    ALLOCATE(this%forbidden(1000))
    this%forbidden(:) = .FALSE.

    N_files = 7 !**** To change if you add some file *****
    ALLOCATE(list_names(N_files))
    IF (.NOT.(ALLOCATED(this%file))) THEN
       ALLOCATE(this%file(size(list_names)))
    ELSE
       WRITE(*,*) '*** Initialize files error: this%file is already allocated. ***'
       error_out = 1
       GOTO 1000 !-- End of subroutine --
    ENDIF

    list_names(1) = trim(this%dir)//'/pos'
    list_names(2) = trim(this%dir)//'/energy.dat'
    list_names(3) = trim(this%dir)//'/info.dat'
    list_names(4) = trim(this%dir)//'/montecarlo.dat'
    list_names(5) = trim(this%dir)//'/potential.dat'
    list_names(6) = trim(this%file_pos)
    list_names(7) = trim(this%dir)//'/output.out'
    !***** Here you add the name of the file ******
    DO I = 1, SIZE(this%file)
       CALL file_constructor(this%file(I), list_names(I), this%forbidden, error_out)
       IF (error_out .NE. 0) THEN
          GOTO 1000 !-- End of subroutine --
       ENDIF
    ENDDO

    !----------- Output directory is created ------------
    CALL check_directory(this%dir, dir_log, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    IF (.NOT.(dir_log)) THEN
       CALL SYSTEM('mkdir '//trim(this%dir))
    ENDIF
    
    !------------ input file is copied in output directory ---------
    CALL SYSTEM('cp input '//trim(this%dir))
    
    !------- Some files are opened -------------
    !-- energy file --
    OPEN(this%file(2)%unit, FILE = trim(this%file(2)%name), &
         FORM='FORMATTED', STATUS='UNKNOWN')

    !-- montecarlo file --
    OPEN(this%file(4)%unit, FILE = trim(this%file(4)%name), &
         FORM='FORMATTED', STATUS='UNKNOWN')

    !-- output file --
    OPEN(this%file(7)%unit, FILE = trim(this%file(7)%name), &
         FORM='FORMATTED', STATUS='UNKNOWN')

    WRITE(*,*) '*** Nothing more will be written in the shell. Output will be written in '
    WRITE(*,*) trim(this%file(7)%name), ' ***'

    WRITE(this%file(7)%unit,*) '*** Files initialized ***'
    
1000 CONTINUE

    IF (ALLOCATED(list_names)) THEN
       DEALLOCATE(list_names)    
    ENDIF

  END SUBROUTINE initialize_files
