! Add temporary debug dump for TRAVEL entries around a sample location to aid review
! This debug output is temporary and will be removed after review.

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
