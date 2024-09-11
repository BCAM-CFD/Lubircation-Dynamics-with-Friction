!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!*********************************************
!  CLASS READ_INPUT FILE
!*********************************************
! This class allows to read variables from an input file.
! In order to use it you have to write, for example:
!  pos = 5
! in your input file, and use it as in the example in main.f90.
! Until now, this class allows to read next types of data
!    real
!    double
!    integer
!    CHARACTER(LEN=MAX_CHAR) 
!    LOGICAL variables.
!    Arrays of integers
!    Arrays of reals
!    Arrays of doubles
! The subroutine to use to read variables from input 
! is read_variable.
!-----------------------------------------------------

module class_read_input
  !---------------------------------
  ! Class to read input files
  !---------------------------------
  IMPLICIT NONE
  INTEGER, PARAMETER :: MAX_CHAR = 1000 !-- To be replaced by the MAX_CHAR of your program --
  TYPE read_input_type
     CHARACTER(LEN=MAX_CHAR) :: file
     CHARACTER(LEN=MAX_CHAR) :: line
     CHARACTER(LEN=MAX_CHAR) :: variable_string
     CHARACTER(LEN=MAX_CHAR) :: variable_name
     CHARACTER(LEN=MAX_CHAR) :: format
     INTEGER :: length_name_variable
  END type read_input_type
  
  !----- INTERFACES ---------
  INTERFACE get_variable
     module PROCEDURE get_variable_real, &
          get_variable_double , &
          get_variable_integer, &
          get_variable_string , &
          get_variable_logical, &
          get_variable_array_integer, &
          get_variable_array_real   , &
          get_variable_array_double
  END INTERFACE get_variable 

  INTERFACE read_variable
     module PROCEDURE read_variable_real, &
          read_variable_double , &
          read_variable_integer, &
          read_variable_string , &
          read_variable_logical, &
          read_variable_array_integer, &
          read_variable_array_real   , &
          read_variable_array_double

  END INTERFACE read_variable 

  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_read_input_constructor.f90'
    include 'inc_find_string.f90'
    include 'inc_obtain_variable_string.f90'
    include 'inc_get_variable.f90'
    include 'inc_get_format.f90'
    include 'inc_take_size_array.f90'
    include 'inc_read_variable.f90'
    include 'inc_search_unit_b.f90'

  END module class_read_input
