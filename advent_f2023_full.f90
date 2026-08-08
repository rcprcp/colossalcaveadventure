! Full conversion attempt of advent.for to Fortran 2023 (stage 2 draft)
module advent_mod
  use iso_fortran_env, only: int64
  use compat_mod, only: i64, ishift64, bitset64, RAN, VOCAB, RSPEAK, SPEAK, PSPEAK, DROP, MOVE, CARRY, GETIN, A5TOA1, POOF, MAINT
  implicit none

  integer, parameter :: i64p = i64

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
    res = bitset64(COND(l), int(n, kind=i64p))
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
    res = (mod(COND(loc), 2_i64) == 0_i64) .and. ((PROP( get_vocab_lamp()) == 0_i64) .or. .not. HERE(get_vocab_lamp(), loc))
  end function DARK

  pure function PCT(n) result(res)
    integer(kind=i64p), intent(in) :: n
    logical :: res
    res = (RAN(100) < n)
  end function PCT

  ! Helper to return VOCAB LAMP object number (we call VOCAB function)
  pure function get_vocab_lamp() result(id)
    integer(kind=i64p) :: id
    id = VOCAB(0 + ia5('LAMP'), 1)
  end function get_vocab_lamp

  ! Minimal translator for 5-char literals to integer used by VOCAB indices
  pure function ia5(str) result(val)
    character(len=*), intent(in) :: str
    integer(kind=i64p) :: val
    ! This is a stub to keep the original expressions compiling. Real VOCAB mapping
    ! relies on the KTAB/ATAB table; here we return 0 to avoid accidental uses.
    val = 0_i64
  end function ia5

end module advent_mod

program advent_main
  use advent_mod
  use compat_mod
  implicit none

  print *, 'Starting Fortran2023 converted ADVENT (stage 2 draft)'
  print *, 'Note: many external routines remain as placeholders. Further testing required.'

  ! The original program immediately did initialization based on SETUP
  ! We'll perform a simple startup message and call POOF (as original did)
  call POOF()
  print *, 'Scaffolding initialization complete. Convert remaining code in subsequent commits.'

end program advent_main
