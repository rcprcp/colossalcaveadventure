! Full conversion attempt of advent.for to Fortran 2023 (stage 2 draft)
module advent_mod
  use iso_fortran_env, only: int64
  use compat_mod, only: i64, ishift64, bitset64, RAN, RSPEAK, SPEAK, PSPEAK, MOVE, CARRY, GETIN, A5TOA1, POOF, MAINT, BUG
  implicit none

  integer, parameter :: i64p = i64

  ! Config: data filename (use advent.dat as requested)
  character(len=128) :: DATA_FILENAME = 'advent.dat'

  ! Size parameters (from original DATA statements)
  integer(kind=i64p), parameter :: LINSIZ = 9650_i64
  integer(kind=i64p), parameter :: TRVSIZ = 750_i64
  integer(kind=i64p), parameter :: TABSIZ = 300_i64
  integer(kind=i64p), parameter :: LOCSIZ = 150_i64
  integer(kind=i64p), parameter :: VRBSIZ = 35_i64
  integer(kind=i64p), parameter :: RTXSIZ = 205_i64
  integer(kind=i64p), parameter :: CLSMAX = 12_i64
  integer(kind=i64p), parameter :: HNTSIZ = 20_i64
  integer(kind=i64p), parameter :: MAGSIZ = 35_i64

  ! Common / arrays converted to module variables
  integer(kind=i64p) :: LINES(LINSIZ)
  integer(kind=i64p) :: TRAVEL(TRVSIZ)
  integer(kind=i64p) :: KTAB(TABSIZ), ATAB(TABSIZ)
  integer(kind=i64p) :: LTEXT(LOCSIZ), STEXT(LOCSIZ), KEY(LOCSIZ), COND(LOCSIZ), ABB(LOCSIZ), ATLOC(LOCSIZ)

  integer(kind=i64p) :: PLAC(100), PLACE(100), FIXD(100), FIXED(100), LINK(200), PTEXT(100), PROP(100)
  integer(kind=i64p) :: ACTSPK(VRBSIZ)
  integer(kind=i64p) :: RTEXT(RTXSIZ)
  integer(kind=i64p) :: CTEXT(CLSMAX), CVAL(CLSMAX)
  integer(kind=i64p) :: HINTLC(HNTSIZ), HINTED(HNTSIZ), HINTS(HNTSIZ,4)
  integer(kind=i64p) :: MTEXT(MAGSIZ)
  integer(kind=i64p) :: TK(20), DLOC(6), ODLOC(6)
  character(len=5) :: HNAME(4)

  ! Simple text storage for WIP: store message lines as strings (one per record)
  character(len=256) :: TEXT_LINES(LINSIZ)
  integer(kind=i64p) :: LINUSE = 1_i64
  integer(kind=i64p) :: CLSSES = 1_i64
  integer(kind=i64p) :: OLDLOC = -1_i64
  integer(kind=i64p) :: TRVS = 1_i64
  integer(kind=i64p) :: HNTMAX = 0_i64

  ! Logical flags
  logical :: DSEEN(6), BLKLIN, HINTED_LOGICAL, YES, START
  logical :: WZDARK, LMWARN, CLOSNG, PANIC, CLOSED, GAVEUP, SCORNG, DEMO, YEA

  integer(kind=i64p) :: IDONDX

contains

  pure function TOTING(obj) result(res)
    integer(kind=i64p), intent(in) :: obj
    logical :: res
    res = (PLACE(obj) == -1_i64)
  end function TOTING

  pure function HERE(obj, loc) result(res)
    integer(kind=i64p), intent(in) :: obj, loc
    logical :: res
    res = (PLACE(obj) == loc) .or. TOTING(obj)
  end function HERE

  pure function AT(obj, loc) result(res)
    integer(kind=i64p), intent(in) :: obj, loc
    logical :: res
    res = (PLACE(obj) == loc) .or. (FIXED(obj) == loc)
  end function AT

  pure function BITSET_fn(l, n) result(res)
    integer(kind=i64p), intent(in) :: l, n
    logical :: res
    ! Use compat_mod.bitset64 which handles bounds
    res = bitset64(COND(l), int(n))
  end function BITSET_fn

  pure function FORCED(loc) result(res)
    integer(kind=i64p), intent(in) :: loc
    logical :: res
    res = (COND(loc) == 2_i64)
  end function FORCED

  pure function DARK(loc) result(res)
    integer(kind=i64p), intent(in) :: loc
    logical :: res
    ! PROP(LAMP) == 0 means lamp has no battery? Align with original: DARK(DUMMY)=MOD(COND(LOC),2).EQ.0.AND.(PROP(LAMP).EQ.0.OR.
    !                                                    .NOT.HERE(LAMP))
    res = (mod(COND(loc), 2_i64) == 0_i64) .and. ((PROP(get_vocab_lamp()) == 0_i64) .or. .not. HERE(get_vocab_lamp(), loc))
  end function DARK

  function PCT(n) result(res)
    integer(kind=i64p), intent(in) :: n
    logical :: res
    res = (RAN(100_i64) < n)
  end function PCT

  ! Helper to return VOCAB LAMP object number (we call VOCAB function defined below)
  pure function get_vocab_lamp() result(id)
    integer(kind=i64p) :: id
    id = VOCAB(ia5('LAMP'), 1_i64)
  end function get_vocab_lamp

  ! Minimal translator for 5-char literals to integer used by VOCAB indices
  pure function ia5(str) result(val)
    character(len=*), intent(in) :: str
    integer(kind=i64p) :: val
    integer :: i, clen
    character(len=5) :: s
    val = 0_i64
    s = adjustl(str)//repeat(' ', max(0,5-len_trim(str)))
    do i = 1, 5
      val = val * 256_i64 + int(iachar(s(i:i)), kind=i64p)
    end do
  end function ia5

  ! VOCAB lookup: given a packed 5-char integer 'a' and a type b (0 motion,1 object,2 action,3 special)
  ! Return KTAB value (the full numeric code) or -1 if not found.
  pure function VOCAB(a, b) result(res)
    integer(kind=i64p), intent(in) :: a, b
    integer(kind=i64p) :: res
    integer :: i
    integer(kind=i64p) :: phrog, decoded

    ! pack 'PHROG' same way
    phrog = ia5('PHROG')

    res = -1_i64
    do i = 1, TABSIZ
      if (KTAB(i) == 0_i64) cycle
      if (KTAB(i) == -1_i64) exit
      decoded = ieor(ATAB(i), phrog)
      if (decoded == a .and. (KTAB(i) / 1000_i64) == b) then
        res = KTAB(i)
        return
      end if
    end do
  end function VOCAB

  ! Parse integers from a line string into arr(1..maxn), return count in nfound
  subroutine parse_integers(line, arr, maxn, nfound)
    character(len=*), intent(in) :: line
    integer(kind=i64p), intent(out) :: arr(*)
    integer(kind=i64p), intent(in) :: maxn
    integer(kind=i64p), intent(out) :: nfound
    integer :: pos, len_line, start, ios, i
    character(len=256) :: token
    integer(kind=i64p) :: val
    integer :: count

    len_line = len_trim(line)
    pos = 1
    count = 0

    do while (pos <= len_line .and. count < int(maxn))
      ! skip spaces
      do while (pos <= len_line .and. line(pos:pos) == ' ')
        pos = pos + 1
      end do
      if (pos > len_line) exit
      start = pos
      do while (pos <= len_line .and. line(pos:pos) /= ' ')
        pos = pos + 1
      end do
      token = adjustl(line(start:pos-1))
      read(token, *, iostat=ios) val
      if (ios == 0) then
        count = count + 1
        arr(count) = val
      end if
    end do

    nfound = count
  end subroutine parse_integers

  ! helper to convert integer iostat to string for messages
  pure function itoa(i) result(s)
    integer, intent(in) :: i
    character(len=12) :: s
    write(s, '(I0)') i
  end function itoa

end module advent_mod
