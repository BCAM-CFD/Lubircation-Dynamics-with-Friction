!---------------------------------------------------------------------
! This code has been developed  in collaboration between
! - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
! - Sagaya Savarimuthu Prasanna Kumar, from Marco Ellero's group.
! - Jose Antonio Ruiz-Lopez, from Marco Ellero's group.
!    Currently from the Department of Applied Physics at
!    Universidad de Granada.
! - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developers: Adolfo Vazquez-Quesada.
!             Sagaya Savarimuthu Prasanna Kumar.
!             Jose Antonio Ruiz Lopez.
!---------------------------------------------------------------------
module class_particle
  !---------------------------------
  ! Class of the particles.
  !---------------------------------
  use class_computational
  IMPLICIT NONE
  
  TYPE particle_type

     !-- Basic physic variables --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: pos    !-- position --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: vel    !-- velocity --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: Omega  !-- angular velocity --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: force  !-- force ---
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: torque !-- torque
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: acc    !-- acc ---     
     REAL(Pr)                              :: R      !-- Radius ---
     REAL(Pr)                              :: mass   !-- mass --
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp    !-- interpart. stress ---
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_rep   !-- interpart. stress (rep forces)---
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_con   !-- interpart. stress (contact forces)---
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_lub_norm   !-- interpart. stress (normal lub forces)---
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_lub_tang   !-- interpart. stress (tang lub forces) ---

     !-- Neighbours variables --
     INTEGER, DIMENSION(:) , ALLOCATABLE :: neigh_list !-- neighbours list --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos0       !-- Last position before updating neighbours --
     INTEGER                             :: N_neigh    !-- Number of neighbours --
     
     !-- Contact list --
     INTEGER, DIMENSION(:) , ALLOCATABLE   :: cont_list             !-- contact list
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: def_tan               !-- Accumulative tangential contact deformation beween particles --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: def_topw, def_botw    !Accumulative tagential contact deformation with walls --
     INTEGER                               :: N_cont                !-- Number of contacts --

     !-- Others --
     LOGICAL :: bulk               !-- If we consider it is in the bulk region --

  END type particle_type

  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_particle_constructor.f90'
    include 'inc_particle_info.f90'
    include 'inc_particle_destructor.f90'
 
END module class_particle
