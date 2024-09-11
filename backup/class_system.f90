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

module class_system
  !---------------------------------
  ! Class of the total system
  !---------------------------------
  use class_computational
  use class_particle
  use class_random
  use class_output
  use class_cell
  use class_wall
  IMPLICIT NONE
  
  TYPE system_type
     !-- Physics --
     TYPE(particle_type), DIMENSION(:), ALLOCATABLE :: part !-- array of particles --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: L !-- Box size --
     INTEGER         :: N    !-- Number of particles --
     INTEGER         :: dim  !-- Number of dimensions --
     REAL(Pr)        :: eta0 !-- Shear viscosity --
     REAL(Pr)        :: eta1 !-- Shear viscosity at high shear rates
                             !   (biviscous model) --
     REAL(Pr)        :: R_par     !-- particle size --
     REAL(Pr)        :: I_par     !-- moment of inertia of the particle --
     REAL(Pr)        :: a_par     !-- particle reduced size --
     REAL(Pr)        :: R_ave     !-- average particle distribution size --
     REAL(Pr)        :: gamma_dot0 !-- Shear rate --
     REAL(Pr)        :: gamma_dot_critical !-- critical Shear rate --
     REAL(Pr)        :: V         !-- particle relative velocity --
     REAL(Pr)        :: gamma_dot !-- imposed shear rate --
     REAL(Pr)        :: calc_gamma_dot !-- calculated shear rate --
     LOGICAL         :: biviscous !-- if the fluid is biviscous --
     TYPE(wall_type) :: wall !-- wall variable --
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp !-- Stress part-part--
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_con !-- Stress part-part contact --
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_rep !-- Stress part-part repulsive --
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_lub !-- Stress part-part lubrication --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: force_plane_x !-- Force on a x plane --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: force_plane_y !-- Force on a y plane --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: force_plane_z !-- Force on a z plane --
     REAL(Pr) :: d_bulk  !-- Distance from the walls where we consider the 
                         !   system as bulk (in Lz units) --
     REAL(Pr) :: L_bulk  !-- Distance from the walls where we consider the 
                         !   system as bulk --
     REAL(Pr) :: V_bulk   !-- Bulk volume --

     !-- Interparticle interactions --
     REAL(Pr) :: rcut       !-- cutoff radius of the interactions --
     REAL(Pr) :: rcut_sq    !-- rcut * rcut
     REAL(Pr) :: rcut_on    !-- cuton radius of the interactions --
     REAL(Pr) :: rcut_on_sq !-- rcut_on * rcut_on --
     REAL(Pr) :: F0_rep     !-- F0 of the repulsive force --
     REAL(Pr) :: tau_rep    !-- tau of the repulsive force --
     REAL(Pr) :: Fcn        !-- F0 of the contact force --
     REAL(Pr) :: Fct        !-- Tangential contact force --
     REAL(Pr) :: Fdm	    !-- Damping contact force --
     REAL(Pr) :: mus        !-- Static Friction Coefficient --
     REAL(Pr) :: mud        !-- Dynamic Friction Coefficient --
     REAL(Pr) :: Ep         !-- Reduced Young's modulus
     REAL(Pr) :: rough    !-- Mean roughness of the particle --
     REAL(Pr) :: Somc       !-- Critical Sommerfeld Number --
     REAL(Pr) :: Bp         !-- Parameter Stribeck curve --
     
     ! -- Contacts and overlaps --
     REAL(Pr) :: Ncont		!-- Number of particles in contacts --
     REAL(Pr) :: Ovpc		!-- Overlap percentage max. --
     REAL(Pr) :: Ovpclim	!-- Overlap percentage lim. --
     REAL(Pr) :: defmax		!-- deformation percentage max. --
     REAL(Pr) :: deflim		!-- deformation percentage lim. --

     !-- Time steps --
     INTEGER  :: Nsteps !-- Number of steps --
     REAL(Pr) :: dt     !-- time step --
     REAL(Pr) :: dt_sq  !-- dt * dt --

     !--- Neighbours --
     REAL(Pr) :: rlist  !-- rmax / rcut --
     REAL(Pr) :: rmax   !-- radius for searching neighbours (rl from
                        ! Allen and Tildesley) --
     REAL(Pr) :: rmax_sq !-- rmax * rmax --
     INTEGER  :: Npart_max !-- Maximum number of particles per cell --
     INTEGER  :: Nmax_list !-- Maximum number of neighbours --
     INTEGER, DIMENSION(3) :: Ncells !-- Number of cells --
     TYPE(cell_type), DIMENSION(:,:,:), ALLOCATABLE :: cell !-- Array of cells --
     LOGICAL  :: update_neigh !-- For checking if the neighbours
                              !   should be updated --
     
     !-- Computational scheme --
     LOGICAL  :: explicit     !-- If the scheme is explicit or
                              !   semi-implicit --
     INTEGER  :: N_sweep      !-- Number of sweeps of the
                              !   semi-implicit method --
     REAL(Pr) :: sweep_tol    !-- Tolerance of the semi-implicit
                              !   method--
     INTEGER  :: N_sweep_max  !-- Maximum allowed number of sweeps --

     !-- Files --
     CHARACTER(LEN=MAX_CHAR) :: dir_output !-- Dir to output --
     LOGICAL                 :: read_pos   !-- If the positions are
                                           !   read from a file --
     LOGICAL                 :: read_rad   !-- If the radii are
                                           !   read from a file --                                           
     LOGICAL                 :: read_vel   !-- If the vels are
                                           !   read from a file --
     LOGICAL                 :: read_Omega !-- If the angular velocities are
                                           !   read from a file --
     CHARACTER(LEN=MAX_CHAR) :: file_pos   !-- File with the initial
                                           !   positions --
     CHARACTER(LEN=MAX_CHAR) :: file_rad   !-- File with the initial
                                           !   positions --
     CHARACTER(LEN=MAX_CHAR) :: file_vel   !-- File with the initial
                                           !   velocities --
     CHARACTER(LEN=MAX_CHAR) :: file_Omega !-- File with the initial
                                           !   angular velocities --
     TYPE(output_type)       :: output     !-- Output object --
     
     !-- Others --
     TYPE(random_type) :: random !-- Random object --
     REAL(Pr)          :: pi     !-- 3.141592654... --
     
  END type system_type

  !------- SUBROUTINES AND FUNCTIONS --------------
CONTAINS
  !-- Initial state --
  include 'inc_create_system.f90'
  include 'inc_initial_positions.f90'
  include 'inc_initial_radius.f90'
  include 'inc_initial_mass.f90'
  include 'inc_initial_velocities.f90'
  include 'inc_initial_angular_velocities.f90'
  include 'inc_initial_cells.f90'
  include 'inc_cells_edges.f90'
  include 'inc_cells_neighbours.f90'
  include 'inc_initial_neigh_list.f90'

  !-- Writting output --
  include 'inc_write_info.f90'
  include 'inc_write_particles.f90'
  include 'inc_write_particles_info.f90'
  include 'inc_write_cells_info.f90'
  include 'inc_write_neighbours.f90'
  include 'inc_write_wall.f90'
  include 'inc_write_Nsweeps.f90'
  include 'inc_write_stress.f90'
  include 'inc_write_force_plane.f90'
  include 'inc_write_shear_rate.f90'
  include 'inc_calc_partShear.f90'

  !-- Searching neighbours --
  include 'inc_calculate_cells_list.f90'
  include 'inc_calculate_neigh_list.f90'
  include 'inc_calculate_neigh.f90'
  include 'inc_check_update_neigh.f90'

  !-- Moving the simulation --
  include 'inc_forces.f90'
  include 'inc_friction_coeff.f90'
  include 'inc_forces_explicit.f90'
  include 'inc_VV.f90'
  include 'inc_VV_explicit.f90'
  include 'inc_PBC.f90'
  include 'inc_vel_implicit_lub.f90'
  include 'inc_vel_implicit_lub_biviscous.f90'
  include 'inc_vel_iterative.f90'
  include 'inc_vel_iterative_biviscous.f90'

  !-- Checking --
  include 'inc_check_pos_walls.f90'
  include 'inc_check_part_dist.f90'

  !-- Calculating some other things differences --
  include 'inc_bulk.f90'
  include 'inc_stress.f90'
  include 'inc_force_plane.f90'
  include 'inc_compute_shear_rate.f90'

  !-- Finishing --
  include 'inc_destroy_system.f90'  
 
END module class_system
