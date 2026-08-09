! Advent Fortran2023 scaffolding (updated)
module compat_mod
  use iso_fortran_env, only: int64, real64
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
      ! use ishft to form mask (portable)
      mask = ishft(1_i64, n)
      res = iand(cond, mask) /= 0_i64
    end if
  end function bitset64

  ! Random integer in [0,n-1] returned as 64-bit integer
  integer(kind=i64) function RAN(n)
    integer(kind=i64), intent(in) :: n
    real(kind=real64) :: r
    call random_number(r)
    if (n <= 0_i64) then
      RAN = 0_i64
    else
      RAN = int(r * real(n, kind=real64), kind=i64)
    end if
  end function RAN

  ! Minimal VOCAB stub used during scaffolding. Returns 0 for unknown (64-bit)
  integer(kind=i64) function VOCAB(a,b)
    integer(kind=i64), intent(in) :: a,b
    VOCAB = 0_i64
  end function VOCAB

  ! Simple BUG handler: print message and stop (keeps original interface minimal)
  subroutine BUG(code)
    integer(kind=i64), intent(in) :: code
    print '(A,I0)', 'BUG called with code ', code
    stop 'BUG'
  end subroutine BUG

  ! Placeholder stubs for external procedures used by the original program.
  subroutine RSPEAK(i)
    integer(kind=i64), intent(in) :: i
    ! placeholder: speak a message index
  end subroutine RSPEAK

  subroutine SPEAK(idx)
    integer(kind=i64), intent(in) :: idx
    ! placeholder
  end subroutine SPEAK

  subroutine PSPEAK(obj, kk)
    integer(kind=i64), intent(in) :: obj, kk
    ! placeholder
  end subroutine PSPEAK

  subroutine DROP(obj, loc)
    integer(kind=i64), intent(in) :: obj, loc
    ! placeholder
  end subroutine DROP

  subroutine MOVE(obj, loc)
    integer(kind=i64), intent(in) :: obj, loc
    ! placeholder
  end subroutine MOVE

  subroutine CARRY(obj, loc)
    integer(kind=i64), intent(in) :: obj, loc
    ! placeholder
  end subroutine CARRY

  subroutine GETIN(w1, w1x, w2, w2x)
    character(len=*), intent(out) :: w1
    integer(kind=i64), intent(out) :: w1x, w2x
    character(len=*), intent(out) :: w2
    ! minimal placeholder; return empty words
    if (len(w1) > 0) w1 = ''
    if (len(w2) > 0) w2 = ''
    w1x = 0_i64; w2x = 0_i64
  end subroutine GETIN

  subroutine A5TOA1(a5, a5x, filler, tk, k)
    character(len=*), intent(in) :: a5
    integer(kind=i64), intent(in) :: a5x
    character(len=*), intent(in) :: filler
    character(len=*), intent(out) :: tk
    integer(kind=i64), intent(in) :: k
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

! Debug module: provides temporary debugging output
module debug_mod
  use iso_fortran_env, only: int64
  implicit none
contains
  subroutine dump_travel_sample(KEY, TRAVEL, loc)
    integer(kind=int64), intent(in) :: KEY(:), TRAVEL(:)
    integer(kind=int64), intent(in) :: loc
    integer(kind=int64) :: k, idx, val, cnt
    if (loc < 1 .or. loc > size(KEY)) then
      print '(A,I0)', 'dump_travel_sample: invalid loc ', loc
      return
    end if
    k = KEY(int(loc))
    if (k == 0_int64) then
      print '(A,I0)', 'dump_travel_sample: no travel entries for loc ', loc
      return
    end if
    print '(A,I0)', 'dump_travel_sample: travel entries for loc ', loc
    idx = int(k)
    cnt = 0_int64
    do while (TRAVEL(idx) /= 0_int64)
      val = TRAVEL(idx)
      print '(A,I0, A, I0)', '  entry ', cnt, ': raw=', val, ' -> dest=', abs(val)/1000
      cnt = cnt + 1_int64
      if (val < 0_int64) exit
      idx = idx + 1
      if (idx > size(TRAVEL)) exit
    end do
  end subroutine dump_travel_sample
end module debug_mod

program advent_scaffold
  use compat_mod
  implicit none
  print *, 'Fortran2023 scaffolding for advent: compat_mod updated.'
  print *, 'Stage 1 scaffolding improved: RAN/VOCAB/BUG use 64-bit kinds.'
end program advent_scaffold
