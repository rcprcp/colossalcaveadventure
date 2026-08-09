! Colossal Cave Adventure – shared data module (Fortran 2023 port)
!
! This module holds ALL shared game data: the database arrays and the
! game-state flags/counters.  It does NOT contain I/O or game-logic; those
! live in io_mod, data_mod, and game_mod respectively.
module advent_mod
  use iso_fortran_env, only: int64
  implicit none

  ! Convenience alias – all integers stored in 64 bits to match original
  ! PDP-10 word size.
  integer, parameter :: i64p = int64

  ! Config
  character(len=128) :: DATA_FILENAME = 'advent.dat'

  ! ---- Size constants (from original DATA statements) ----
  integer(kind=i64p), parameter :: LINSIZ = 9650_i64p
  integer(kind=i64p), parameter :: TRVSIZ = 750_i64p
  integer(kind=i64p), parameter :: TABSIZ = 300_i64p
  integer(kind=i64p), parameter :: LOCSIZ = 150_i64p
  integer(kind=i64p), parameter :: VRBSIZ = 35_i64p
  integer(kind=i64p), parameter :: RTXSIZ = 205_i64p
  integer(kind=i64p), parameter :: CLSMAX = 12_i64p
  integer(kind=i64p), parameter :: HNTSIZ = 20_i64p
  integer(kind=i64p), parameter :: MAGSIZ = 35_i64p

  ! ---- Database arrays ----
  integer(kind=i64p) :: LINES(LINSIZ)      = 0_i64p
  integer(kind=i64p) :: TRAVEL(TRVSIZ)     = 0_i64p
  integer(kind=i64p) :: KTAB(TABSIZ)       = 0_i64p
  integer(kind=i64p) :: ATAB(TABSIZ)       = 0_i64p
  integer(kind=i64p) :: LTEXT(LOCSIZ)      = 0_i64p
  integer(kind=i64p) :: STEXT(LOCSIZ)      = 0_i64p
  integer(kind=i64p) :: KEY(LOCSIZ)        = 0_i64p
  integer(kind=i64p) :: COND(LOCSIZ)       = 0_i64p
  integer(kind=i64p) :: ABB(LOCSIZ)        = 0_i64p
  integer(kind=i64p) :: ATLOC(LOCSIZ)      = 0_i64p

  integer(kind=i64p) :: PLAC(100)          = 0_i64p  ! initial object locations
  integer(kind=i64p) :: PLACE(100)         = 0_i64p  ! current object locations
  integer(kind=i64p) :: FIXD(100)          = 0_i64p  ! initial fixed locations
  integer(kind=i64p) :: FIXED(100)         = 0_i64p  ! current fixed locations
  integer(kind=i64p) :: LINK(200)          = 0_i64p  ! object chain at location
  integer(kind=i64p) :: PTEXT(100)         = 0_i64p  ! object description pointers
  integer(kind=i64p) :: PROP(100)          = 0_i64p  ! object properties

  integer(kind=i64p) :: ACTSPK(VRBSIZ)    = 0_i64p  ! default action messages
  integer(kind=i64p) :: RTEXT(RTXSIZ)     = 0_i64p  ! random message pointers
  integer(kind=i64p) :: CTEXT(CLSMAX)     = 0_i64p  ! class message pointers
  integer(kind=i64p) :: CVAL(CLSMAX)      = 0_i64p  ! class score thresholds
  integer(kind=i64p) :: HINTLC(HNTSIZ)   = 0_i64p  ! hint location counters
  integer(kind=i64p) :: HINTS(HNTSIZ,4)  = 0_i64p  ! hint data
  integer(kind=i64p) :: MTEXT(MAGSIZ)    = 0_i64p  ! magic message pointers

  integer(kind=i64p) :: DLOC(6)           = 0_i64p  ! dwarf locations
  integer(kind=i64p) :: ODLOC(6)          = 0_i64p  ! previous dwarf locations

  ! Parallel text storage: human-readable lines keyed by LINES index
  character(len=256) :: TEXT_LINES(LINSIZ) = ' '

  ! ---- Database-loading state (used only during read_database) ----
  integer(kind=i64p) :: LINUSE   = 1_i64p
  integer(kind=i64p) :: CLSSES   = 1_i64p   ! class message counter
  integer(kind=i64p) :: OLDLOC_DB = -1_i64p  ! "previous loc" during db load
  integer(kind=i64p) :: TRVS     = 1_i64p
  integer(kind=i64p) :: HNTMAX   = 0_i64p

  ! ---- Game-state flags ----
  logical :: DSEEN(6)        = .false.
  logical :: BLKLIN          = .true.
  logical :: HINTED(HNTSIZ) = .false.  ! logical: true if hint taken
  logical :: WZDARK          = .false.
  logical :: LMWARN          = .false.
  logical :: CLOSNG          = .false.
  logical :: PANIC           = .false.
  logical :: CLOSED          = .false.
  logical :: GAVEUP          = .false.
  logical :: SCORNG          = .false.

  ! ---- Numeric game-state variables ----
  integer(kind=i64p) :: LIMIT  = 330_i64p   ! lamp lifetime
  integer(kind=i64p) :: HOLDNG = 0_i64p     ! number of objects being carried
  integer(kind=i64p) :: IDONDX = 0_i64p     ! scratch loop variable

  ! ---- Class count (set during database load) ----
  integer(kind=i64p) :: CLSSES_TOTAL = 0_i64p

contains

  ! -----------------------------------------------------------------------
  ! ia5: pack a 5-character string into a 64-bit integer (big-endian bytes).
  ! Used for vocabulary lookups.
  ! -----------------------------------------------------------------------
  pure function ia5(str) result(val)
    character(len=*), intent(in) :: str
    integer(kind=i64p) :: val
    integer :: i
    character(len=5) :: s
    integer :: slen
    val  = 0_i64p
    slen = min(len_trim(str), 5)
    s    = str(1:slen)//repeat(' ', 5-slen)
    do i = 1, 5
      val = val * 256_i64p + int(iachar(s(i:i)), kind=i64p)
    end do
  end function ia5

  ! -----------------------------------------------------------------------
  ! parse_integers: read whitespace-separated integers from a text line.
  ! Used by section_parsers during database loading.
  ! -----------------------------------------------------------------------
  subroutine parse_integers(line, arr, maxn, nfound)
    character(len=*), intent(in)    :: line
    integer(kind=i64p), intent(out) :: arr(*)
    integer(kind=i64p), intent(in)  :: maxn
    integer(kind=i64p), intent(out) :: nfound
    integer :: pos, len_line, start_pos, ios
    character(len=256) :: token
    integer(kind=i64p) :: val
    integer :: count

    len_line = len_trim(line)
    pos = 1; count = 0
    do while (pos <= len_line .and. count < int(maxn))
      do while (pos <= len_line .and. (line(pos:pos) == ' ' .or. line(pos:pos) == char(9))); pos = pos + 1; end do
      if (pos > len_line) exit
      start_pos = pos
      do while (pos <= len_line .and. line(pos:pos) /= ' ' .and. line(pos:pos) /= char(9)); pos = pos + 1; end do
      token = adjustl(line(start_pos:pos-1))
      read(token, *, iostat=ios) val
      if (ios == 0) then; count = count + 1; arr(count) = val; end if
    end do
    nfound = count
  end subroutine parse_integers

end module advent_mod
