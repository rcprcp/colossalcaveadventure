! data_mod.f90 – data-structure routines for Colossal Cave Adventure
!
! Implements: VOCAB, DSTROY, JUGGLE, MOVE, PUT, CARRY, DROP
!
! These are exact functional translations of the subroutines in advent.for
! (starting at line 2344).

module data_mod
  use iso_fortran_env, only: int64
  use advent_mod,      only: i64p, KTAB, ATAB, TABSIZ, ATLOC, LINK, &
                              PLACE, FIXED, HOLDNG, ia5
  implicit none

contains

  ! -----------------------------------------------------------------------
  ! VOCAB(id, init): look up a 5-char packed word in the vocabulary table.
  !   id   – packed 5-char word (ia5 encoding)
  !   init – ≥ 0: only match words of type init (ktab/1000==init), return mod 1000
  !           -1: match any type, return full ktab value or -1 if not found
  ! -----------------------------------------------------------------------
  integer(kind=i64p) function VOCAB(id, init)
    integer(kind=i64p), intent(in) :: id, init
    integer(kind=i64p) :: phrog, hash
    integer :: i

    phrog = ia5('PHROG')
    hash  = ieor(id, phrog)

    do i = 1, TABSIZ
      if (KTAB(i) == -1_i64p) exit
      if (KTAB(i) == 0_i64p) cycle
      if (init >= 0_i64p .and. KTAB(i) / 1000_i64p /= init) cycle
      if (ATAB(i) == hash) then
        if (init >= 0_i64p) then
          VOCAB = mod(KTAB(i), 1000_i64p)
        else
          VOCAB = KTAB(i)
        end if
        return
      end if
    end do
    VOCAB = -1_i64p
  end function VOCAB


  ! -----------------------------------------------------------------------
  ! DSTROY(object): permanently eliminate object by moving it to loc 0.
  ! -----------------------------------------------------------------------
  subroutine DSTROY(object)
    integer(kind=i64p), intent(in) :: object
    call MOVE(object, 0_i64p)
  end subroutine DSTROY


  ! -----------------------------------------------------------------------
  ! JUGGLE(object): pick up and put down to move to front of location chain.
  ! -----------------------------------------------------------------------
  subroutine JUGGLE(object)
    integer(kind=i64p), intent(in) :: object
    integer(kind=i64p) :: iloc, jloc
    iloc = PLACE(int(object))
    jloc = FIXED(int(object))
    call MOVE(object, iloc)
    call MOVE(object + 100_i64p, jloc)
  end subroutine JUGGLE


  ! -----------------------------------------------------------------------
  ! MOVE(object, where): place any object anywhere.
  ! -----------------------------------------------------------------------
  subroutine MOVE(object, where)
    integer(kind=i64p), intent(in) :: object, where
    integer(kind=i64p) :: from

    if (object > 100_i64p) then
      from = FIXED(int(object - 100_i64p))
    else
      from = PLACE(int(object))
    end if
    if (from > 0_i64p .and. from <= 300_i64p) call CARRY(object, from)
    call DROP(object, where)
  end subroutine MOVE


  ! -----------------------------------------------------------------------
  ! PUT(object, where, pval): same as MOVE, but returns -1-pval.
  ! -----------------------------------------------------------------------
  integer(kind=i64p) function PUT(object, where, pval)
    integer(kind=i64p), intent(in) :: object, where, pval
    call MOVE(object, where)
    PUT = -1_i64p - pval
  end function PUT


  ! -----------------------------------------------------------------------
  ! CARRY(object, where): start toting an object, removing it from loc list.
  ! -----------------------------------------------------------------------
  subroutine CARRY(object, where)
    integer(kind=i64p), intent(in) :: object, where
    integer(kind=i64p) :: temp

    if (object > 100_i64p) then
      ! Moving the "fixed" second location – just remove from ATLOC
      if (ATLOC(int(where)) /= object) then
        temp = ATLOC(int(where))
        do while (LINK(int(temp)) /= object)
          temp = LINK(int(temp))
        end do
        LINK(int(temp)) = LINK(int(object))
      else
        ATLOC(int(where)) = LINK(int(object))
      end if
      return
    end if

    if (PLACE(int(object)) == -1_i64p) return   ! already toting
    PLACE(int(object)) = -1_i64p
    HOLDNG = HOLDNG + 1_i64p

    if (where <= 0_i64p) return
    if (ATLOC(int(where)) == object) then
      ATLOC(int(where)) = LINK(int(object))
      return
    end if
    temp = ATLOC(int(where))
    do while (LINK(int(temp)) /= object)
      temp = LINK(int(temp))
    end do
    LINK(int(temp)) = LINK(int(object))
  end subroutine CARRY


  ! -----------------------------------------------------------------------
  ! DROP(object, where): place an object at a location, prefixing ATLOC list.
  ! -----------------------------------------------------------------------
  subroutine DROP(object, where)
    integer(kind=i64p), intent(in) :: object, where

    if (object > 100_i64p) then
      FIXED(int(object - 100_i64p)) = where
    else
      if (PLACE(int(object)) == -1_i64p) HOLDNG = HOLDNG - 1_i64p
      PLACE(int(object)) = where
    end if

    if (where <= 0_i64p) return
    if (int(where) < 1 .or. int(where) > size(ATLOC)) return
    LINK(int(object)) = ATLOC(int(where))
    ATLOC(int(where)) = object
  end subroutine DROP

end module data_mod
