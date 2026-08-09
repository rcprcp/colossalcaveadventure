! compat_mod.f90 – core utility functions for the Fortran 2023 ADVENT port.
!
! Contains ONLY the non-stub, pure utility routines that have no dependencies
! on the game data (advent_mod) or game logic (game_mod / io_mod).
!
! Compile this module first.
module compat_mod
  use iso_fortran_env, only: int64, real64
  implicit none
  integer, parameter :: i64 = int64

contains

  ! -----------------------------------------------------------------------
  ! ishift64: portable left/right logical shift on 64-bit integers.
  ! Positive n → left shift; negative n → right shift (logical).
  ! -----------------------------------------------------------------------
  pure function ishift64(x, n) result(r)
    integer(kind=i64), intent(in) :: x
    integer,           intent(in) :: n
    integer(kind=i64) :: r
    r = ishft(x, n)
  end function ishift64

  ! -----------------------------------------------------------------------
  ! bitset64: test whether bit n (0-based) is set in 'cond'.
  ! -----------------------------------------------------------------------
  pure function bitset64(cond, n) result(res)
    integer(kind=i64), intent(in) :: cond
    integer,           intent(in) :: n
    logical :: res
    integer(kind=i64) :: mask
    if (n < 0 .or. n >= bit_size(cond)) then
      res = .false.
    else
      mask = ishft(1_i64, n)
      res  = iand(cond, mask) /= 0_i64
    end if
  end function bitset64

  ! -----------------------------------------------------------------------
  ! RAN(n): return a pseudo-random integer in [0, n-1].
  ! -----------------------------------------------------------------------
  integer(kind=i64) function RAN(n)
    integer(kind=i64), intent(in) :: n
    real(kind=real64) :: r
    call random_number(r)
    if (n <= 0_i64) then
      RAN = 0_i64
    else
      RAN = int(r * real(n, kind=real64), kind=i64)
      if (RAN >= n) RAN = n - 1_i64
    end if
  end function RAN

  ! -----------------------------------------------------------------------
  ! BUG(code): fatal error handler.  Prints code and stops.
  ! -----------------------------------------------------------------------
  subroutine BUG(code)
    integer(kind=i64), intent(in) :: code
    print '(A)', ' Fatal error, see source code for interpretation.'
    print '(A)', ' Probable cause: erroneous info in database.'
    print '(A,I3)', ' Error code =', int(code)
    stop 'BUG'
  end subroutine BUG

end module compat_mod

