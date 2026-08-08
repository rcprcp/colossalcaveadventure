! Advent Fortran2023 scaffolding
module compat_mod
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: i64 = int64
contains
  pure function ishift64(x, n) result(r)
    integer(kind=i64), intent(in) :: x
    integer, intent(in) :: n
    integer(kind=i64) :: r
    ! Use standard ishft with 64-bit integers. Ishft shifts left for positive n, right for negative n.
    r = ishft(x, n)
  end function ishift64

  pure function bitset64(cond, n) result(res)
    integer(kind=i64), intent(in) :: cond
    integer, intent(in) :: n
    logical :: res
    integer(kind=i64) :: mask
    if (n < 0 .or. n >= bit_size(cond)) then
      res = .false.
    else
      mask = shiftl(1_i64, n)
      res = iand(cond, mask) /= 0_i64
    end if
  end function bitset64

  ! Random integer in [0,n-1]
  integer function RAN(n)
    integer, intent(in) :: n
    real :: r
    call random_number(r)
    if (n <= 0) then
      RAN = 0
    else
      RAN = int(r * real(n))
    end if
  end function RAN

  ! Minimal VOCAB stub used during scaffolding. Returns 0 for unknown.
  integer function VOCAB(a,b)
    integer, intent(in) :: a,b
    VOCAB = 0
  end function VOCAB

  ! Placeholder stubs for external procedures used by the original program.
  subroutine RSPEAK(i)
    integer, intent(in) :: i
    ! placeholder: speak a message index
  end subroutine RSPEAK

  subroutine SPEAK(idx)
    integer, intent(in) :: idx
    ! placeholder
  end subroutine SPEAK

  subroutine PSPEAK(obj, kk)
    integer, intent(in) :: obj, kk
    ! placeholder
  end subroutine PSPEAK

  subroutine DROP(obj, loc)
    integer, intent(in) :: obj, loc
    ! placeholder
  end subroutine DROP

  subroutine MOVE(obj, loc)
    integer, intent(in) :: obj, loc
    ! placeholder
  end subroutine MOVE

  subroutine CARRY(obj, loc)
    integer, intent(in) :: obj, loc
    ! placeholder
  end subroutine CARRY

  subroutine GETIN(w1, w1x, w2, w2x)
    character(len=*), intent(out) :: w1
    integer, intent(out) :: w1x, w2x
    character(len=*), intent(out) :: w2
    ! minimal placeholder; return empty words
    if (len(w1) > 0) w1 = ''
    if (len(w2) > 0) w2 = ''
    w1x = 0; w2x = 0
  end subroutine GETIN

  subroutine A5TOA1(a5, a5x, filler, tk, k)
    character(len=*), intent(in) :: a5
    integer, intent(in) :: a5x
    character(len=*), intent(in) :: filler
    character(len=*), intent(out) :: tk
    integer, intent(in) :: k
    ! placeholder
    if (len(tk) > 0) tk = ''
  end subroutine A5TOA1

  subroutine POOF()
    ! placeholder
  end subroutine POOF

  subroutine MAINT()
    ! placeholder for maintenance mode
  end subroutine MAINT

end module compat_mod

program advent_scaffold
  use compat_mod
  implicit none
  print *, 'Fortran2023 scaffolding for advent: compat_mod added.'
  print *, 'This is stage 1: compatibility module and placeholder stubs.  '
  print *, 'Next stage will convert advent.for into a full Fortran 2023 program using 64-bit integers.'
end program advent_scaffold
