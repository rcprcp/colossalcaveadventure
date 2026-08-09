! section_parsers_mod: database-section I/O handlers extracted from advent_mod.
! Each subroutine reads one section of the ADVENT data file and populates the
! shared module-level arrays declared in advent_mod.
module section_parsers_mod
  use iso_fortran_env, only: int64
  use compat_mod,      only: BUG, ishift64, bitset64
  use advent_mod
  implicit none

contains

  ! Read the ADVENT database file; dispatch each section to its handler.
  subroutine read_database()
    integer(kind=int64) :: sect
    integer :: ios

    open(unit=1, file=trim(DATA_FILENAME), status='old', action='read', iostat=ios)
    if (ios /= 0) then
      print '(A)', 'Error: could not open ADVENT data file: '//trim(DATA_FILENAME)
      return
    end if

    ! Initialise counters (mirrors original behaviour).
    LINUSE = 1_int64
    TRVS   = 1_int64
    CLSSES = 1_int64
    OLDLOC = -1_int64

    do
      read(1, *, iostat=ios) sect
      if (ios /= 0) exit
      select case (sect)
      case (0_int64)
        exit
      case (1_int64)
        call handle_section_messages(1_int64)
      case (2_int64)
        call handle_section_messages(2_int64)
      case (3_int64)
        call handle_travel_table()
      case (4_int64)
        call handle_vocabulary()
      case (5_int64)
        call handle_section_messages(5_int64)
      case (6_int64)
        call handle_section_messages(6_int64)
      case (7_int64)
        call handle_object_locations()
      case (8_int64)
        call handle_action_defaults()
      case (9_int64)
        call handle_liquids()
      case (10_int64)
        call handle_section_messages(10_int64)
      case (11_int64)
        call handle_hints()
      case (12_int64)
        call handle_section_messages(12_int64)
      case default
        call BUG(22_int64)
      end select
    end do

    close(1)
    print *, 'Finished reading ADVENT database from ', trim(DATA_FILENAME)
  end subroutine read_database

  ! Sections 1, 2, 5, 6, 10, 12: text message lines.
  ! Reads LOC + text, packs text into LINES[], and sets the appropriate pointer
  ! array entry (LTEXT/STEXT/PTEXT/RTEXT/CTEXT/MTEXT) on first encounter per LOC.
  subroutine handle_section_messages(n)
    integer(kind=int64), intent(in) :: n
    character(len=256) :: rec, s, rest, chunk
    character(len=32) :: intstr
    integer(kind=int64) :: loc, lenr, nchunks, startpos, endpos
    integer(kind=int64) :: p, next, k, m, pack, pointer_val
    integer :: ios, pos

    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(0_int64)
        end if
      end if
      s = adjustl(rec)
      read(s, *, iostat=ios) loc
      if (ios /= 0) call BUG(1_int64)
      if (loc == -1_int64) exit

      ! Extract the text after the leading integer.
      write(intstr, '(I0)') loc
      pos = index(s, adjustl(intstr))
      if (pos > 0) then
        rest = adjustl(s(pos + len_trim(adjustl(intstr)) :))
      else
        rest = ''
      end if
      rest = trim(rest)

      lenr = len_trim(rest)
      if (lenr <= 0_int64) then
        nchunks = 0_int64
      else
        nchunks = ((lenr - 1_int64) / 5_int64) + 1_int64
      end if

      p    = LINUSE
      next = LINUSE + 1_int64
      if (next + nchunks - 1_int64 > LINSIZ) call BUG(2_int64)

      if (int(p) >= 1 .and. int(p) <= LINSIZ) TEXT_LINES(int(p)) = rest

      ! Pack each 5-char chunk into a 64-bit integer.
      do k = 1_int64, nchunks
        startpos = (k - 1_int64) * 5_int64 + 1_int64
        endpos   = min(startpos + 4_int64, lenr)
        chunk = ''
        if (lenr >= startpos) chunk = rest(int(startpos):int(endpos))
        if (len_trim(chunk) < 5) chunk = chunk // repeat(' ', 5 - len_trim(chunk))
        pack = 0_int64
        do m = 1_int64, 5_int64
          pack = pack * 256_int64 + int(iachar(chunk(int(m):int(m))), kind=int64)
        end do
        if (int(next) >= 1 .and. int(next) <= LINSIZ) LINES(int(next)) = pack
        next = next + 1_int64
      end do

      pointer_val = next
      if (int(p) >= 1 .and. int(p) <= LINSIZ) then
        if (loc /= OLDLOC) then
          LINES(int(p)) = -pointer_val
        else
          LINES(int(p)) = pointer_val
        end if
      end if

      ! Set the section-specific text pointer on first encounter.
      select case (n)
      case (1_int64)
        if (int(loc) >= 1 .and. int(loc) <= LOCSIZ) then
          if (LTEXT(int(loc)) == 0_int64) LTEXT(int(loc)) = p
        end if
      case (2_int64)
        if (int(loc) >= 1 .and. int(loc) <= LOCSIZ) then
          if (STEXT(int(loc)) == 0_int64) STEXT(int(loc)) = p
        end if
      case (5_int64)
        if (int(loc) >= 1 .and. int(loc) <= 100) PTEXT(int(loc)) = p
      case (6_int64)
        if (int(loc) >= 1 .and. int(loc) <= RTXSIZ) RTEXT(int(loc)) = p
      case (10_int64)
        if (CLSSES > CLSMAX) call BUG(6_int64)
        if (int(CLSSES) >= 1 .and. int(CLSSES) <= CLSMAX) then
          CTEXT(int(CLSSES)) = p
          CVAL(int(CLSSES))  = loc
        end if
        CLSSES = CLSSES + 1_int64
      case (12_int64)
        if (int(loc) >= 1 .and. int(loc) <= MAGSIZ) MTEXT(int(loc)) = p
      end select

      LINUSE = next
      if (LINUSE <= LINSIZ) LINES(int(LINUSE)) = -1_int64
      OLDLOC = loc
    end do
  end subroutine handle_section_messages

  ! Section 3: travel table.
  subroutine handle_travel_table()
    character(len=256) :: rec
    integer(kind=int64), allocatable :: vals(:)
    integer(kind=int64) :: nvals, loc, newloc, i, tk
    integer :: ios

    allocate(vals(22))
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(3_int64)
        end if
      end if

      call parse_integers(rec, vals, 22_int64, nvals)
      if (nvals == 0_int64) cycle
      loc = vals(1)
      if (loc == -1_int64) exit
      if (loc == 0_int64) cycle
      if (nvals < 2_int64) call BUG(4_int64)
      newloc = vals(2)

      if (int(loc) >= 1 .and. int(loc) <= LOCSIZ) then
        if (KEY(int(loc)) == 0_int64) then
          KEY(int(loc)) = TRVS
        else
          if (int(TRVS-1) >= 1 .and. int(TRVS-1) <= TRVSIZ) then
            TRAVEL(int(TRVS-1)) = -TRAVEL(int(TRVS-1))
          end if
        end if
      end if

      do i = 3_int64, nvals
        tk = vals(int(i))
        if (tk == 0_int64) exit
        if (int(TRVS) >= 1 .and. int(TRVS) <= TRVSIZ) then
          TRAVEL(int(TRVS)) = newloc * 1000_int64 + tk
        end if
        TRVS = TRVS + 1_int64
        if (TRVS == TRVSIZ) call BUG(3_int64)
      end do

      ! Mark last entry negative per original convention.
      if (TRVS > 1_int64) then
        if (int(TRVS-1) >= 1 .and. int(TRVS-1) <= TRVSIZ) then
          TRAVEL(int(TRVS-1)) = -TRAVEL(int(TRVS-1))
        end if
      end if
    end do

    deallocate(vals)
  end subroutine handle_travel_table

  ! Section 4: vocabulary (KTAB/ATAB).
  subroutine handle_vocabulary()
    character(len=256) :: rec
    character(len=32) :: intstr
    character(len=5) :: w
    integer :: ios, tabndx, pos
    integer(kind=int64) :: n, packed, phrog

    phrog  = ia5('PHROG')
    tabndx = 1

    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(5_int64)
        end if
      end if
      if (len_trim(rec) == 0) cycle
      read(rec, *, iostat=ios) n
      if (ios /= 0) call BUG(6_int64)
      if (n == -1_int64) then
        if (tabndx >= 1 .and. tabndx <= TABSIZ) KTAB(tabndx) = -1_int64
        exit
      end if
      write(intstr, '(I0)') n
      pos = index(adjustl(rec), adjustl(intstr))
      if (pos > 0) then
        w = adjustl(rec(pos + len_trim(adjustl(intstr)) :))
      else
        w = '     '
      end if
      w = adjustl(w) // repeat(' ', max(0, 5 - len_trim(w)))
      w = w(1:5)
      packed = ia5(w)
      if (tabndx >= 1 .and. tabndx <= TABSIZ) then
        ATAB(tabndx) = ieor(packed, phrog)
        KTAB(tabndx) = n
      end if
      tabndx = tabndx + 1
      if (tabndx > TABSIZ) call BUG(4_int64)
    end do
  end subroutine handle_vocabulary

  ! Section 7: object initial locations (PLAC/FIXD).
  subroutine handle_object_locations()
    character(len=256) :: rec
    integer(kind=int64), allocatable :: vals(:)
    integer(kind=int64) :: nvals, obj, j, k
    integer :: ios

    allocate(vals(4))
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(10_int64)
        end if
      end if
      call parse_integers(rec, vals, 4_int64, nvals)
      if (nvals == 0_int64) cycle
      obj = vals(1)
      if (obj == -1_int64) exit
      j = 0_int64
      k = 0_int64
      if (nvals >= 2_int64) j = vals(2)
      if (nvals >= 3_int64) k = vals(3)
      if (obj >= 1_int64 .and. obj <= 100_int64) then
        PLAC(int(obj)) = j
        FIXD(int(obj)) = k
      end if
    end do
    deallocate(vals)
  end subroutine handle_object_locations

  ! Section 8: action-default messages (ACTSPK).
  subroutine handle_action_defaults()
    character(len=256) :: rec
    integer(kind=int64), allocatable :: vals(:)
    integer(kind=int64) :: nvals, verb, j
    integer :: ios

    allocate(vals(4))
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(11_int64)
        end if
      end if
      call parse_integers(rec, vals, 4_int64, nvals)
      if (nvals == 0_int64) cycle
      verb = vals(1)
      if (verb == -1_int64) exit
      j = 0_int64
      if (nvals >= 2_int64) j = vals(2)
      if (verb >= 1_int64 .and. verb <= VRBSIZ) ACTSPK(int(verb)) = j
    end do
    deallocate(vals)
  end subroutine handle_action_defaults

  ! Section 9: conditional bits / liquid properties (COND).
  subroutine handle_liquids()
    character(len=256) :: rec
    integer(kind=int64), allocatable :: vals(:)
    integer(kind=int64) :: nvals, k, loc
    integer :: ios, i

    allocate(vals(22))
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(12_int64)
        end if
      end if
      call parse_integers(rec, vals, 22_int64, nvals)
      if (nvals == 0_int64) cycle
      k = vals(1)
      if (k == -1_int64) exit
      if (k == 0_int64) cycle
      do i = 2, int(nvals)
        loc = vals(i)
        if (loc == 0_int64) exit
        if (int(loc) >= 1 .and. int(loc) <= LOCSIZ) then
          if (BITSET_fn(loc, k)) call BUG(8_int64)
          COND(int(loc)) = COND(int(loc)) + ishift64(1_int64, int(k))
        end if
      end do
    end do
    deallocate(vals)
  end subroutine handle_liquids

  ! Section 11: hints.
  subroutine handle_hints()
    character(len=256) :: rec
    integer(kind=int64), allocatable :: vals(:)
    integer(kind=int64) :: nvals, k
    integer :: ios, i

    allocate(vals(8))
    do
      read(1, '(A)', iostat=ios) rec
      if (ios /= 0) then
        if (ios > 0) then
          exit
        else
          call BUG(13_int64)
        end if
      end if
      call parse_integers(rec, vals, 8_int64, nvals)
      if (nvals == 0_int64) cycle
      k = vals(1)
      if (k == -1_int64) exit
      if (k == 0_int64) cycle
      if (k < 0_int64 .or. k > HNTSIZ) call BUG(7_int64)
      do i = 1, 4
        if (int(k) >= 1 .and. int(k) <= HNTSIZ) then
          if (i + 1 <= int(nvals)) then
            HINTS(int(k), i) = vals(i + 1)
          else
            HINTS(int(k), i) = 0_int64
          end if
        end if
      end do
      HNTMAX = max(HNTMAX, k)
    end do
    deallocate(vals)
  end subroutine handle_hints

  ! Finalise database: build ATLOC/LINK lists, copy PLACE/FIXED, set forced-motion
  ! COND bits, and initialise PROP and treasure tally.
  subroutine finalize_database()
    integer(kind=int64) :: i, k, MAXTRS, TALLY, I_idx

    do i = 1, 100
      LINK(i)     = 0_int64
      LINK(i+100) = 0_int64
      PLACE(i)    = 0_int64
      PROP(i)     = 0_int64
      FIXED(i)    = FIXD(i)
    end do
    do i = 1, LOCSIZ
      ATLOC(i) = 0_int64
      ABB(i)   = 0_int64
      if (LTEXT(i) == 0_int64 .or. KEY(i) == 0_int64) cycle
      k = int(KEY(i))
      if (k >= 1 .and. k <= TRVSIZ) then
        if (mod(abs(TRAVEL(k)), 1000_int64) == 1_int64) COND(int(i)) = 2_int64
      end if
    end do

    ! Place two-location objects first (FIXD > 0).
    do i = 1, 100
      k = 101_int64 - i
      if (FIXD(int(k)) > 0_int64) then
        call drop_internal(k + 100_int64, FIXD(int(k)))
        call drop_internal(k, PLAC(int(k)))
      end if
    end do

    ! Place all remaining objects.
    do i = 1, 100
      k = 101_int64 - i
      PLACE(int(k)) = PLAC(int(k))
      FIXED(int(k)) = FIXD(int(k))
      if (PLAC(int(k)) /= 0_int64 .and. FIXD(int(k)) <= 0_int64) then
        call drop_internal(k, PLAC(int(k)))
      end if
    end do

    MAXTRS = 79_int64
    TALLY  = 0_int64
    do I_idx = 50_int64, MAXTRS
      if (int(I_idx) >= 1 .and. int(I_idx) <= 100) then
        if (PTEXT(int(I_idx)) /= 0_int64) PROP(int(I_idx)) = -1_int64
        TALLY = TALLY - PROP(int(I_idx))
      end if
    end do
  end subroutine finalize_database

  ! Internal helper: place object in location, updating LINK and ATLOC.
  subroutine drop_internal(obj, loc)
    integer(kind=int64), intent(in) :: obj, loc
    if (loc <= 0_int64) return
    if (obj < 1_int64 .or. obj > 200_int64) return
    if (int(obj) < 1 .or. int(obj) > size(LINK)) return
    if (int(loc) < 1 .or. int(loc) > size(ATLOC)) return
    LINK(int(obj))   = ATLOC(int(loc))
    ATLOC(int(loc))  = obj
    PLACE(int(obj))  = loc
  end subroutine drop_internal

end module section_parsers_mod
