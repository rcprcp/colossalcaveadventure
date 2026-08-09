! Full conversion attempt of advent.for to Fortran 2023 (stage 2 draft)
module advent_mod
  use iso_fortran_env, only: int64
  use compat_mod, only: i64, ishift64, bitset64, RAN, RSPEAK, SPEAK, PSPEAK, DROP, MOVE, CARRY, GETIN, A5TOA1, POOF, MAINT, BUG
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
    res = (mod(COND(loc), 2_i64) == 0_i64) .and. ((PROP(get_vocab_lamp()) == 0_i64) .or. .not. HERE(get_vocab_lamp(), loc))
  end function DARK

  pure function PCT(n) result(res)
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

  ! Read the ADVENT database file (refactored from computed GOTO loop in original)
  subroutine read_database()
    integer(kind=i64p) :: sect
    integer :: ios

    open(unit=1, file=trim(DATA_FILENAME), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      print '(A)', 'Error: could not open ADVENT data file: '//trim(DATA_FILENAME)
      return
    end if

    ! initialize counters (mirrors original behavior)
    LINUSE = 1_i64
    TRVS = 1_i64
    CLSSES = 1_i64
    OLDLOC = -1_i64

    do
      read(1, *, iostat=ios) sect
      if (ios /= 0) then
        print '(A)', 'End of data or read error (iostat=' // trim(adjustl(itoa(ios))) // ')' 
        exit
      end if
      ! Original used a computed GOTO based on SECT; we use SELECT CASE instead
      select case (sect)
      case (0_i64)
        ! SECTION 0: end of database
        exit
      case (1_i64)
        call handle_section_messages(1_i64)
      case (2_i64)
        call handle_section_messages(2_i64)
      case (3_i64)
        call handle_travel_table()
      case (4_i64)
        call handle_vocabulary()
      case (5_i64)
        call handle_section_messages(5_i64)
      case (6_i64)
        call handle_section_messages(6_i64)
      case (7_i64)
        call handle_object_locations()
      case (8_i64)
        call handle_action_defaults()
      case (9_i64)
        call handle_liquids()
      case (10_i64)
        call handle_section_messages(10_i64)
      case (11_i64)
        call handle_hints()
      case (12_i64)
        call handle_section_messages(12_i64)
      case default
        call BUG(22_i64)
      end select
    end do

    close(1)
    print *, 'Finished reading ADVENT database from ', trim(DATA_FILENAME)
  end subroutine read_database

  ! Handler: parse sections 1,2,5,6,10,12 and store lines into TEXT_LINES and
  ! reconstruct original pointer encoding in LINES[] so legacy code can use it.
  subroutine handle_section_messages(n)
    integer(kind=i64p), intent(in) :: n
    character(len=256) :: rec, s, rest, chunk
    integer(kind=i64p) :: loc, lenr, nchunks, startpos, endpos
    integer(kind=i64p) :: p, next, k, m
    integer :: ios

    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          ! non-fatal: end of file
          exit
        else
          call BUG(0_i64)
        end if
      end if
      s = adjustl(rec)
      ! Try to read the leading integer LOC from the line
      read(s, *, iostat=ios) loc
      if (ios /= 0) then
        call BUG(1_i64)
      end if
      if (loc == -1_i64) exit

      ! extract the rest of the line after the integer
      ! write integer to string and find its position
      character(len=32) :: intstr
      write(intstr, '(I0)') loc
      integer :: pos
      pos = index(s, adjustl(intstr))
      if (pos > 0) then
        rest = adjustl(s(pos + len_trim(adjustl(intstr)) :))
      else
        rest = ''
      end if
      rest = trim(rest)

      ! Before storing, check space
      ! compute how many 5-char chunks we'll need
      lenr = len_trim(rest)
      if (lenr <= 0_i64) then
        nchunks = 0_i64
      else
        nchunks = ( (lenr - 1_i64) / 5_i64 ) + 1_i64
      end if

      ! Each record stores a pointer word at position p = LINUSE, followed by nchunks packed words.
      p = LINUSE
      next = LINUSE + 1_i64
      if (next + nchunks - 1_i64 > LINSIZ) call BUG(2_i64)

      ! store the textual copy for human readability
      TEXT_LINES(int(p)) = rest

      ! pack each 5-char chunk into an integer and store in LINES[next ...]
      do k = 1_i64, nchunks
        startpos = (k - 1_i64) * 5_i64 + 1_i64
        endpos = min(startpos + 4_i64, lenr)
        chunk = ''
        if (lenr >= startpos) then
          chunk = rest( int(startpos) : int(endpos) )
        end if
        ! pad to 5 chars
        if (len_trim(chunk) < 5) chunk = chunk // repeat(' ', 5 - len_trim(chunk))
        ! pack into 64-bit integer as big-endian sequence
        integer(kind=i64p) :: pack
        pack = 0_i64
        do m = 1_i64, 5_i64
          pack = pack * 256_i64 + int(iachar(chunk(int(m))), kind=i64p)
        end do
        LINES( int(next) ) = pack
        next = next + 1_i64
      end do

      ! set pointer value to index after last packed word
      integer(kind=i64p) :: pointer_val
      pointer_val = next

      ! if this is the first line for this LOC (LOC != OLDLOC), make pointer negative
      if (loc /= OLDLOC) then
        LINES( int(p) ) = -pointer_val
      else
        LINES( int(p) ) = pointer_val
      end if

      ! set appropriate text pointers (point to the pointer-word index p)
      select case (n)
      case (1_i64)
        if (LTEXT(int(loc)) == 0_i64) LTEXT(int(loc)) = p
      case (2_i64)
        if (STEXT(int(loc)) == 0_i64) STEXT(int(loc)) = p
      case (5_i64)
        if (loc > 0_i64 .and. loc <= 100_i64) PTEXT(int(loc)) = p
      case (6_i64)
        if (loc > 0_i64 .and. loc <= RTXSIZ) RTEXT(int(loc)) = p
      case (10_i64)
        if (CLSSES > CLSMAX) call BUG(6_i64)
        CTEXT( int(CLSSES) ) = p
        CVAL( int(CLSSES) ) = loc
        CLSSES = CLSSES + 1_i64
      case (12_i64)
        if (loc > 0_i64 .and. loc <= MAGSIZ) MTEXT(int(loc)) = p
      end select

      ! advance LINUSE to next free pointer and mark LINES(LINUSE) = -1 (end marker)
      LINUSE = next
      if (LINUSE <= LINSIZ) LINES( int(LINUSE) ) = -1_i64

      OLDLOC = loc

    end do

  end subroutine handle_section_messages

  ! Implement Section 3: travel table parser
  subroutine handle_travel_table()
    character(len=256) :: rec
    integer(kind=i64p), allocatable :: vals(:)
    integer(kind=i64p) :: nvals
    integer(kind=i64p) :: loc, newloc
    integer(kind=i64p) :: i, tk
    integer :: ios

    allocate(vals(22))

    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(3_i64)
        end if
      end if

      call parse_integers(rec, vals, 22_i64, nvals)
      if (nvals == 0_i64) cycle
      loc = vals(1)
      if (loc == -1_i64) then
        ! end of section
        exit
      end if
      if (loc == 0_i64) cycle
      if (nvals < 2_i64) call BUG(4_i64)
      newloc = vals(2)

      if (KEY(int(loc)) == 0_i64) then
        KEY(int(loc)) = TRVS
      else
        TRAVEL(int(TRVS-1)) = -TRAVEL(int(TRVS-1))
      end if

      ! process motion numbers (starting at vals(3) .. vals(nvals))
      do i = 3_i64, nvals
        tk = vals(int(i))
        if (tk == 0_i64) exit
        TRAVEL(int(TRVS)) = newloc * 1000_i64 + tk
        TRVS = TRVS + 1_i64
        if (TRVS == TRVSIZ) call BUG(3_i64)
      end do

      ! mark last entry negative per original
      if (TRVS > 1_i64) TRAVEL(int(TRVS-1)) = -TRAVEL(int(TRVS-1))

    end do

    deallocate(vals)
  end subroutine handle_travel_table

  ! Implement Section 4: vocabulary parsing
  subroutine handle_vocabulary()
    character(len=256) :: rec
    integer :: ios
    integer :: tabndx
    integer(kind=i64p) :: n
    character(len=5) :: w
    integer(kind=i64p) :: packed, phrog

    phrog = ia5('PHROG')

    tabndx = 1
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(5_i64)
        end if
      end if
      if (len_trim(rec) == 0) cycle
      ! parse leading integer and following 5-char word (word may be padded or longer)
      read(rec, *, iostat=ios) n
      if (ios /= 0) call BUG(6_i64)
      if (n == -1_i64) then
        KTAB(tabndx) = -1_i64
        exit
      end if
      ! extract 5-char field after integer; find position of integer in line
      character(len=32) :: intstr
      integer :: pos
      write(intstr, '(I0)') n
      pos = index(adjustl(rec), adjustl(intstr))
      if (pos > 0) then
        w = adjustl(rec(pos + len_trim(adjustl(intstr)) :))
      else
        w = '     '
      end if
      w = adjustl(w)//repeat(' ', max(0,5-len_trim(w)))
      w = w(1:5)
      ! pack into integer
      packed = ia5(w)
      ! apply obfuscation to match original ATAB = ATAB XOR 'PHROG'
      ATAB(tabndx) = ieor(packed, phrog)
      KTAB(tabndx) = n
      tabndx = tabndx + 1
      if (tabndx > TABSIZ) call BUG(4_i64)
    end do

  end subroutine handle_vocabulary

  ! Section 7: object locations (PLAC/FIXD)
  subroutine handle_object_locations()
    character(len=256) :: rec
    integer(kind=i64p), allocatable :: vals(:)
    integer(kind=i64p) :: nvals
    integer(kind=i64p) :: obj, j, k
    integer :: ios

    allocate(vals(4))
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(10_i64)
        end if
      end if
      call parse_integers(rec, vals, 4_i64, nvals)
      if (nvals == 0_i64) cycle
      obj = vals(1)
      if (obj == -1_i64) then
        exit
      end if
      if (nvals >= 2_i64) then
        j = vals(2)
      else
        j = 0_i64
      end if
      if (nvals >= 3_i64) then
        k = vals(3)
      else
        k = 0_i64
      end if
      if (obj >= 1_i64 .and. obj <= 100_i64) then
        PLAC(int(obj)) = j
        FIXD(int(obj)) = k
      end if
    end do
    deallocate(vals)
  end subroutine handle_object_locations

  ! Section 8: action defaults (ACTSPK)
  subroutine handle_action_defaults()
    character(len=256) :: rec
    integer(kind=i64p), allocatable :: vals(:)
    integer(kind=i64p) :: nvals
    integer(kind=i64p) :: verb, j
    integer :: ios

    allocate(vals(4))
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(11_i64)
        end if
      end if
      call parse_integers(rec, vals, 4_i64, nvals)
      if (nvals == 0_i64) cycle
      verb = vals(1)
      if (verb == -1_i64) exit
      if (nvals >= 2_i64) then
        j = vals(2)
      else
        j = 0_i64
      end if
      if (verb >= 1_i64 .and. verb <= VRBSIZ) then
        ACTSPK(int(verb)) = j
      end if
    end do
    deallocate(vals)
  end subroutine handle_action_defaults

  ! Section 9: liquids / COND bits
  subroutine handle_liquids()
    character(len=256) :: rec
    integer(kind=i64p), allocatable :: vals(:)
    integer(kind=i64p) :: nvals
    integer(kind=i64p) :: k, loc
    integer :: ios, i

    allocate(vals(22))
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(12_i64)
        end if
      end if
      call parse_integers(rec, vals, 22_i64, nvals)
      if (nvals == 0_i64) cycle
      k = vals(1)
      if (k == -1_i64) exit
      if (k == 0_i64) cycle
      do i = 2, nvals
        loc = vals(int(i))
        if (loc == 0_i64) exit
        if (BITSET_fn(loc, k)) call BUG(8_i64)
        COND(int(loc)) = COND(int(loc)) + ishift64(1_i64, int(k))
      end do
    end do
    deallocate(vals)
  end subroutine handle_liquids

  ! Section 11: hints
  subroutine handle_hints()
    character(len=256) :: rec
    integer(kind=i64p), allocatable :: vals(:)
    integer(kind=i64p) :: nvals
    integer(kind=i64p) :: k
    integer :: ios, i

    allocate(vals(8))
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(13_i64)
        end if
      end if
      call parse_integers(rec, vals, 8_i64, nvals)
      if (nvals == 0_i64) cycle
      k = vals(1)
      if (k == -1_i64) exit
      if (k == 0_i64) cycle
      if (k < 0_i64 .or. k > HNTSIZ) call BUG(7_i64)
      do i = 1, 4
        if (i+1 <= nvals) then
          HINTS(int(k), i) = vals(i+1)
        else
          HINTS(int(k), i) = 0_i64
        end if
      end do
      HNTMAX = max(HNTMAX, k)
    end do
    deallocate(vals)
  end subroutine handle_hints

  ! helper to convert integer iostat to string for messages
  pure function itoa(i) result(s)
    integer, intent(in) :: i
    character(len=12) :: s
    write(s, '(I0)') i
  end function itoa

end module advent_mod

program advent_main
  use advent_mod
  use compat_mod
  use debug_mod
  implicit none

  print *, 'Starting Fortran2023 converted ADVENT (stage 2 WIP)'
  print *, 'Reading ADVENT data file: ', trim(DATA_FILENAME)

  call read_database()

  ! temporary debug dump of travel entries for location 15
  call dump_travel_sample(KEY, TRAVEL, 15_int64)

  print *, 'Database read (WIP). Continuing conversion in further commits.'

  ! The original program immediately did initialization based on SETUP
  ! We'll call POOF as a placeholder
  call POOF()
  print *, 'WIP scaffolding initialization complete.'

end program advent_main
