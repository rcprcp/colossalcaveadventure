! io_mod.f90 – I/O routines for Colossal Cave Adventure (Fortran 2023 port)
!
! Implements: SPEAK, PSPEAK, RSPEAK, MSPEAK, GETIN, YES, YESM, YESX
!
! The original packed-word LINES array stores text as a linked list of
! 5-character chunks packed into 36-bit PDP-10 words.  In this port the array
! is kept as 64-bit integers but the packing convention is the same:
!   LINES(N)  = pointer word:
!     < 0  → another line of the same paragraph follows at -LINES(N)
!     ≥ 0  → this is the last line pointer
!   LINES(N+1..ptr-1) = 5-char chunks packed big-endian in 64 bits
!
! For human readability this port also maintains the TEXT_LINES character
! array (filled in parallel by section_parsers) so we can print ASCII directly.

module io_mod
  use iso_fortran_env, only: int64
  use compat_mod,      only: BUG
  use advent_mod,      only: i64p, LINES, LINSIZ, RTEXT, PTEXT, MTEXT, &
                              TEXT_LINES, BLKLIN
  implicit none

contains

  ! -----------------------------------------------------------------------
  ! SPEAK(N): print the message whose pointer word is at LINES(N).
  ! Precede with a blank line unless BLKLIN is .false.
  ! -----------------------------------------------------------------------
  subroutine SPEAK(n)
    integer(kind=i64p), intent(in) :: n
    integer(kind=i64p) :: k, next_slot

    if (n == 0_i64p) return
    if (n < 1_i64p .or. n > LINSIZ) return

    ! Check for suppressed message ('>$<' sentinel in text)
    if (trim(TEXT_LINES(int(n))) == '>$<') return

    if (BLKLIN) print '()'

    k = n
    do
      if (k < 1_i64p .or. k > LINSIZ) exit
      ! Print this line's text from the parallel TEXT_LINES array
      print '(1X,A)', trim(TEXT_LINES(int(k)))
      ! next_slot = abs(pointer_word) = first empty slot after text chunks
      next_slot = abs(LINES(int(k)))
      if (next_slot < 1_i64p .or. next_slot > LINSIZ) exit
      ! Continue if next slot's pointer is non-negative (continuation line)
      if (LINES(int(next_slot)) < 0_i64p) exit   ! next paragraph or end
      k = next_slot
    end do
  end subroutine SPEAK


  ! -----------------------------------------------------------------------
  ! RSPEAK(i): print the i-th random message (section 6).
  ! -----------------------------------------------------------------------
  subroutine RSPEAK(i)
    integer(kind=i64p), intent(in) :: i
    if (i /= 0_i64p) call SPEAK(RTEXT(int(i)))
  end subroutine RSPEAK


  ! -----------------------------------------------------------------------
  ! MSPEAK(i): print the i-th magic message (section 12).
  ! -----------------------------------------------------------------------
  subroutine MSPEAK(i)
    integer(kind=i64p), intent(in) :: i
    if (i /= 0_i64p) call SPEAK(MTEXT(int(i)))
  end subroutine MSPEAK


  ! -----------------------------------------------------------------------
  ! PSPEAK(msg, skip): find the (skip+1)-th message for object msg and print.
  ! msg  = index into PTEXT (inventory message pointer).
  ! skip = property value (-1 = print inventory line itself).
  ! -----------------------------------------------------------------------
  subroutine PSPEAK(msg, skip)
    integer(kind=i64p), intent(in) :: msg, skip
    integer(kind=i64p) :: m
    integer :: i
    if (msg < 1_i64p .or. msg > 100_i64p) return
    m = PTEXT(int(msg))
    if (m == 0_i64p) return
    if (skip >= 0_i64p) then
      do i = 0, int(skip)
        ! advance through linked list: abs(LINES(m)) is next-block pointer
        do while (LINES(int(m)) >= 0_i64p)
          m = abs(LINES(int(m)))
        end do
        m = abs(LINES(int(m)))
      end do
    end if
    call SPEAK(m)
  end subroutine PSPEAK


  ! -----------------------------------------------------------------------
  ! GETIN: read one command line, return up to two 5-letter words.
  ! word1/word2   – first 5 characters, lower-cased
  ! word1x/word2x – next 5 characters (for long-word echo), as integer
  ! -----------------------------------------------------------------------
  subroutine GETIN(word1, word1x, word2, word2x)
    character(len=5), intent(out) :: word1, word2
    integer(kind=i64p), intent(out) :: word1x, word2x
    character(len=132) :: line
    integer :: ios, i, wstart, wend, wlen
    character(len=5) :: ws(2)
    integer :: nwords, p, llen

    if (BLKLIN) print '()'

    do
      write(*, '("> ")', advance='no')
      read(*, '(A)', iostat=ios) line
      if (ios /= 0) then
        word1 = '     '; word2 = '     '
        word1x = 0_i64p; word2x = 0_i64p
        return
      end if
      ! strip trailing whitespace
      llen = len_trim(line)
      if (llen > 0) exit
    end do

    ! Convert to upper-case for classic 5-letter matching
    do i = 1, llen
      if (line(i:i) >= 'a' .and. line(i:i) <= 'z') then
        line(i:i) = achar(iachar(line(i:i)) - 32)
      end if
    end do

    nwords = 0
    ws(1) = '     '; ws(2) = '     '

    p = 1
    do while (p <= llen .and. nwords < 2)
      ! skip spaces
      do while (p <= llen .and. line(p:p) == ' ')
        p = p + 1
      end do
      if (p > llen) exit
      wstart = p
      do while (p <= llen .and. line(p:p) /= ' ')
        p = p + 1
      end do
      wend = p - 1
      wlen = wend - wstart + 1
      nwords = nwords + 1
      if (wlen >= 5) then
        ws(nwords) = line(wstart:wstart+4)
      else
        ws(nwords) = line(wstart:wend)//repeat(' ', 5-wlen)
      end if
    end do

    word1 = ws(1)
    word2 = ws(2)
    word1x = 0_i64p
    word2x = 0_i64p
  end subroutine GETIN


  ! -----------------------------------------------------------------------
  ! YES(x,y,z): print message x, wait for yes/no, print y or z.
  ! -----------------------------------------------------------------------
  logical function YES(x, y, z)
    integer(kind=i64p), intent(in) :: x, y, z
    YES = YESX(x, y, z, 6)  ! 6 = RTEXT section
  end function YES

  logical function YESM(x, y, z)
    integer(kind=i64p), intent(in) :: x, y, z
    YESM = YESX(x, y, z, 12) ! 12 = MTEXT section
  end function YESM

  logical function YESX(x, y, z, section)
    integer(kind=i64p), intent(in) :: x, y, z
    integer, intent(in) :: section
    character(len=5) :: reply, dummy2
    integer(kind=i64p) :: dx, d2x
    do
      if (x /= 0_i64p) then
        if (section == 12) then
          call MSPEAK(x)
        else
          call RSPEAK(x)
        end if
      end if
      call GETIN(reply, dx, dummy2, d2x)
      if (reply == 'YES  ' .or. reply == 'Y    ') then
        YESX = .true.
        if (y /= 0_i64p) then
          if (section == 12) then; call MSPEAK(y); else; call RSPEAK(y); end if
        end if
        return
      else if (reply == 'NO   ' .or. reply == 'N    ') then
        YESX = .false.
        if (z /= 0_i64p) then
          if (section == 12) then; call MSPEAK(z); else; call RSPEAK(z); end if
        end if
        return
      end if
      print '(A)', ' Please answer the question.'
    end do
  end function YESX


end module io_mod
