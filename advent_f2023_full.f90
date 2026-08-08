! Full conversion attempt of advent.for to Fortran 2023 (stage 2 draft)
module advent_mod
  use iso_fortran_env, only: int64
  use compat_mod, only: i64, ishift64, bitset64, RAN, VOCAB, RSPEAK, SPEAK, PSPEAK, DROP, MOVE, CARRY, GETIN, A5TOA1, POOF, MAINT, BUG
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
    res = (RAN(100_i64) < n)
  end function PCT

  ! Helper to return VOCAB LAMP object number (we call VOCAB function)
  pure function get_vocab_lamp() result(id)
    integer(kind=i64p) :: id
    id = VOCAB(0_i64 + ia5('LAMP'), 1_i64)
  end function get_vocab_lamp

  ! Minimal translator for 5-char literals to integer used by VOCAB indices
  pure function ia5(str) result(val)
    character(len=*), intent(in) :: str
    integer(kind=i64p) :: val
    ! This is a stub to keep the original expressions compiling. Real VOCAB mapping
    ! relies on the KTAB/ATAB table; here we return 0 to avoid accidental uses.
    val = 0_i64
  end function ia5

  ! Read the ADVENT database file (refactored from computed GOTO loop in original)
  subroutine read_database()
    integer(kind=i64p) :: sect
    integer :: ios

    open(unit=1, file=trim(DATA_FILENAME), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      print '(A)', 'Error: could not open ADVENT data file: '//trim(DATA_FILENAME)
      return
    end if

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

  ! Handler: parse sections 1,2,5,6,10,12 and store lines in TEXT_LINES.
  subroutine handle_section_messages(n)
    integer(kind=i64p), intent(in) :: n
    character(len=256) :: rec, s, rest
    integer(kind=i64p) :: loc
    character(len=32) :: intstr
    integer :: ios, pos

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
      write(intstr, '(I0)') loc
      pos = index(s, trim(intstr))
      if (pos > 0) then
        rest = s(pos + len_trim(intstr):)
      else
        rest = s
      end if
      rest = trim(rest)

      if (LINUSE > LINSIZ) call BUG(2_i64)
      TEXT_LINES(LINUSE) = rest

      select case (n)
      case (1_i64)
        if (LTEXT(loc) == 0_i64) LTEXT(loc) = LINUSE
      case (2_i64)
        if (STEXT(loc) == 0_i64) STEXT(loc) = LINUSE
      case (5_i64)
        if (loc > 0_i64 .and. loc <= 100_i64) PTEXT(loc) = LINUSE
      case (6_i64)
        if (loc > 0_i64 .and. loc <= RTXSIZ) RTEXT(loc) = LINUSE
      case (10_i64)
        if (CLSSES > CLSMAX) call BUG(6_i64)
        CTEXT(CLSSES) = LINUSE
        CVAL(CLSSES) = loc
        CLSSES = CLSSES + 1_i64
      case (12_i64)
        if (loc > 0_i64 .and. loc <= MAGSIZ) MTEXT(loc) = LINUSE
      end select

      LINUSE = LINUSE + 1_i64
    end do

  end subroutine handle_section_messages

  subroutine handle_travel_table()
    print '(A)', 'handle_travel_table: not yet implemented (WIP)'
  end subroutine handle_travel_table

  subroutine handle_vocabulary()
    print '(A)', 'handle_vocabulary: not yet implemented (WIP)'
  end subroutine handle_vocabulary

  subroutine handle_object_descriptions()
    print '(A)', 'handle_object_descriptions: not yet implemented (WIP)'
  end subroutine handle_object_descriptions

  subroutine handle_rtext()
    print '(A)', 'handle_rtext: not yet implemented (WIP)'
  end subroutine handle_rtext

  subroutine handle_object_locations()
    print '(A)', 'handle_object_locations: not yet implemented (WIP)'
  end subroutine handle_object_locations

  subroutine handle_action_defaults()
    print '(A)', 'handle_action_defaults: not yet implemented (WIP)'
  end subroutine handle_action_defaults

  subroutine handle_liquids()
    print '(A)', 'handle_liquids: not yet implemented (WIP)'
  end subroutine handle_liquids

  subroutine handle_class_messages()
    print '(A)', 'handle_class_messages: not yet implemented (WIP)'
  end subroutine handle_class_messages

  subroutine handle_hints()
    print '(A)', 'handle_hints: not yet implemented (WIP)'
  end subroutine handle_hints

  subroutine handle_magic_messages()
    print '(A)', 'handle_magic_messages: not yet implemented (WIP)'
  end subroutine handle_magic_messages

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
  implicit none

  print *, 'Starting Fortran2023 converted ADVENT (stage 2 WIP)'
  print *, 'Reading ADVENT data file: ', trim(DATA_FILENAME)

  call read_database()

  print *, 'Database read (WIP). Continuing conversion in further commits.'

  ! The original program immediately did initialization based on SETUP
  ! We'll call POOF as a placeholder
  call POOF()
  print *, 'WIP scaffolding initialization complete.'

end program advent_main
