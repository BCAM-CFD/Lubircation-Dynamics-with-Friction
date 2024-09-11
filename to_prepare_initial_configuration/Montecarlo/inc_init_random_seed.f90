!---------------------------------------------------------------------
! This code has been developed  in collaboration between Marco Ellero,
! leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
! Center  for  Applied  Mathematics)  in  Bilbao,  Spain,  and  Adolfo
! Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
! in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------------------------------------
subroutine init_random_seed(fixed_seed, seed_val)
  !-------------------------------------------------------------
  ! Subrutina para inicializar el generador de numeros aleatorios
  ! intrinseco de fortran. Subrutina basada en
  !   https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
  !-----------------------------------------------------------
  use iso_fortran_env, only: int64
!  use ifport !-- Only for ifort (needed for pid). Comment for gfortran --
  implicit none
  LOGICAL, INTENT(in)    :: fixed_seed
  INTEGER, INTENT(inout) :: seed_val
  integer, allocatable :: seed_var(:) !seed_var instead of seed because od conflicts with ifport
  integer :: i, n, un, istat, dt(8), pid
  integer(int64) :: t

  call random_seed(size = n)
  allocate(seed_var(n))
  ! Fallback to XOR:ing the current time and pid. The PID is
  ! useful in case one launches multiple instances of the same
  ! program in parallel.
  IF (fixed_seed) THEN
     t = seed_val
  ELSE
     call system_clock(t)
     pid = getpid() !-- with ifort you should uncomment the ifport line in the top -- 
     t = ieor(t, int(pid, kind(t)))
     seed_val = t
  ENDIF
  do i = 1, n
     seed_var(i) = lcg(t)
  end do
  call random_seed(put=seed_var)
contains
  ! This simple PRNG might not be good enough for real work, but is
  ! sufficient for seeding a better PRNG.
  function lcg(s)
    integer :: lcg
    integer(int64) :: s
    if (s == 0) then
       s = 104729
    else
       s = mod(s, 4294967296_int64)
    end if
    s = mod(s * 279470273_int64, 4294967291_int64)
    lcg = int(mod(s, int(huge(0), int64)), kind(0))
  end function lcg
end subroutine init_random_seed
