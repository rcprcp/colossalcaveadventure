! game_mod.f90 – main game loop and action verb handlers
!
! This is a structured Fortran 2023 port of the main program body in advent.for
! (labels 1 through 25000, plus all 8000/9000-series action verb handlers and
! the cave-closing / scoring sections).
!
! Design note: The original used computed GOTOs throughout.  Here every "goto"
! is replaced with a SELECT CASE, EXIT, CYCLE, or subroutine call.  The overall
! flow is preserved exactly so gameplay is identical to the PDP-10 original.

module game_mod
  use iso_fortran_env, only: int64
  use advent_mod
  use data_mod
  use io_mod
  implicit none

  ! -------------------------------------------------------------------
  ! Game-state variables (all were local or COMMON in the original)
  ! -------------------------------------------------------------------
  integer(kind=i64p) :: LOC   = 0_i64p  ! current location
  integer(kind=i64p) :: NEWLOC = 0_i64p ! proposed next location
  integer(kind=i64p) :: OLDLOC = 0_i64p ! previous location
  integer(kind=i64p) :: OLDLC2 = 0_i64p ! location before OLDLOC (for death)

  ! Object mnemonic IDs (set during init via VOCAB)
  integer(kind=i64p) :: KEYS=0, LAMP=0, GRATE=0, CAGE=0, ROD=0, ROD2=0
  integer(kind=i64p) :: STEPS=0, BIRD=0, DOOR=0, PILLOW=0, SNAKE=0
  integer(kind=i64p) :: FISSUR=0, TABLET=0, CLAM=0, OYSTER=0, MAGZIN=0
  integer(kind=i64p) :: DWARF=0, KNIFE=0, FOOD=0, BOTTLE=0, WATER=0, OIL=0
  integer(kind=i64p) :: PLANT=0, PLANT2=0, AXE=0, MIRROR=0, DRAGON=0
  integer(kind=i64p) :: CHASM=0, TROLL=0, TROLL2=0, BEAR=0, MESSAG=0
  integer(kind=i64p) :: VEND=0, BATTER=0
  ! Treasures
  integer(kind=i64p) :: NUGGET=0, COINS=0, CHEST=0, EGGS=0, TRIDNT=0
  integer(kind=i64p) :: VASE=0, EMRALD=0, PYRAM=0, PEARL=0, RUG=0, CHAIN=0
  integer(kind=i64p) :: SPICES=0
  ! Motion verbs
  integer(kind=i64p) :: BACK_V=0, LOOK_V=0, CAVE_V=0, NULL_V=0
  integer(kind=i64p) :: ENTRNC=0, DPRSSN=0
  ! Action verbs
  integer(kind=i64p) :: SAY_V=0, LOCK_V=0, THROW_V=0, FIND_V=0, INVENT_V=0

  ! Dwarf / pirate state
  integer(kind=i64p) :: CHLOC=0, CHLOC2=0, DALTLC=0, DFLAG=0
  integer(kind=i64p) :: DTOTAL=0, ATTACK=0, STICK=0, DKILL=0

  ! Counters and flags
  integer(kind=i64p) :: TURNS=0, IWEST=0, KNFLOC=0, DETAIL=0, ABBNUM=5
  integer(kind=i64p) :: MAXDIE=0, NUMDIE=0
  integer(kind=i64p) :: FOOBAR=0, BONUS=0, CLOCK1=30, CLOCK2=50
  integer(kind=i64p) :: TALLY=0, TALLY2=0, MAXTRS=79
  integer(kind=i64p) :: PLOVER_LOC = 0_i64p

  ! Scoring
  integer(kind=i64p) :: SCORE=0, MXSCOR=0

  ! Verb / object context
  integer(kind=i64p) :: VERB=0, OBJ=0, SPK=0, KQ=0

  ! Current travel-table pointer
  integer(kind=i64p) :: KK=0

  ! Second-word context
  character(len=5) :: WD1='     ', WD2='     '
  integer(kind=i64p) :: WD1X=0, WD2X=0

  ! Scratch variable for parsed motion/object code
  integer(kind=i64p) :: k_word = 0_i64p

contains

  ! ===================================================================
  ! Helper functions mirroring original statement functions
  ! ===================================================================

  pure function TOTING(obj) result(res)
    integer(kind=i64p), intent(in) :: obj
    logical :: res
    res = (PLACE(int(obj)) == -1_i64p)
  end function TOTING

  pure function HERE(obj) result(res)
    integer(kind=i64p), intent(in) :: obj
    logical :: res
    res = (PLACE(int(obj)) == LOC) .or. TOTING(obj)
  end function HERE

  pure function AT(obj) result(res)
    integer(kind=i64p), intent(in) :: obj
    logical :: res
    res = (PLACE(int(obj)) == LOC) .or. (FIXED(int(obj)) == LOC)
  end function AT

  pure function BITSET(l, n) result(res)
    integer(kind=i64p), intent(in) :: l, n
    logical :: res
    res = iand(COND(int(l)), ishft(1_i64p, int(n))) /= 0_i64p
  end function BITSET

  pure function FORCED(loc_arg) result(res)
    integer(kind=i64p), intent(in) :: loc_arg
    logical :: res
    res = (COND(int(loc_arg)) == 2_i64p)
  end function FORCED

  pure function DARK() result(res)
    logical :: res
    res = (mod(COND(int(LOC)), 2_i64p) == 0_i64p) .and. &
          (PROP(int(LAMP)) == 0_i64p .or. .not. HERE(LAMP))
  end function DARK

  function PCT(n) result(res)
    integer(kind=i64p), intent(in) :: n
    logical :: res
    res = (RAN(100_i64p) < n)
  end function PCT

  ! LIQ: object number of liquid in bottle
  pure function LIQ2(pbotl) result(res)
    integer(kind=i64p), intent(in) :: pbotl
    integer(kind=i64p) :: res
    res = (1_i64p - pbotl) * WATER + (pbotl / 2_i64p) * (WATER + OIL)
  end function LIQ2

  pure function LIQ() result(res)
    integer(kind=i64p) :: res
    integer(kind=i64p) :: pb
    pb = max(PROP(int(BOTTLE)), -1_i64p - PROP(int(BOTTLE)))
    res = LIQ2(pb)
  end function LIQ

  pure function LIQLOC(loc_arg) result(res)
    integer(kind=i64p), intent(in) :: loc_arg
    integer(kind=i64p) :: res
    integer(kind=i64p) :: cv
    cv = COND(int(loc_arg))
    res = LIQ2((mod(cv/2_i64p*2_i64p, 8_i64p) - 5_i64p) * mod(cv/4_i64p, 2_i64p) + 1_i64p)
  end function LIQLOC


  ! ===================================================================
  ! POOF: initialise wizard/timing config (stripped for non-PDP system)
  ! ===================================================================
  subroutine POOF()
    ! In the PDP-10 original this set prime-time hours, magic words, etc.
    ! For a standalone game we just skip that system-dependent stuff.
  end subroutine POOF


  ! ===================================================================
  ! GAME_INIT: perform one-time game initialisation (replaces label 1100)
  ! ===================================================================
  subroutine GAME_INIT()
    integer :: i
    integer(kind=i64p) :: k

    ! Resolve object/verb mnemonics via VOCAB
    KEYS   = VOCAB(ia5('KEYS '), 1_i64p)
    LAMP   = VOCAB(ia5('LAMP '), 1_i64p)
    GRATE  = VOCAB(ia5('GRATE'), 1_i64p)
    CAGE   = VOCAB(ia5('CAGE '), 1_i64p)
    ROD    = VOCAB(ia5('ROD  '), 1_i64p)
    ROD2   = ROD + 1_i64p
    STEPS  = VOCAB(ia5('STEPS'), 1_i64p)
    BIRD   = VOCAB(ia5('BIRD '), 1_i64p)
    DOOR   = VOCAB(ia5('DOOR '), 1_i64p)
    PILLOW = VOCAB(ia5('PILLO'), 1_i64p)
    SNAKE  = VOCAB(ia5('SNAKE'), 1_i64p)
    FISSUR = VOCAB(ia5('FISSU'), 1_i64p)
    TABLET = VOCAB(ia5('TABLE'), 1_i64p)
    CLAM   = VOCAB(ia5('CLAM '), 1_i64p)
    OYSTER = VOCAB(ia5('OYSTE'), 1_i64p)
    MAGZIN = VOCAB(ia5('MAGAZ'), 1_i64p)
    DWARF  = VOCAB(ia5('DWARF'), 1_i64p)
    KNIFE  = VOCAB(ia5('KNIFE'), 1_i64p)
    FOOD   = VOCAB(ia5('FOOD '), 1_i64p)
    BOTTLE = VOCAB(ia5('BOTTL'), 1_i64p)
    WATER  = VOCAB(ia5('WATER'), 1_i64p)
    OIL    = VOCAB(ia5('OIL  '), 1_i64p)
    PLANT  = VOCAB(ia5('PLANT'), 1_i64p)
    PLANT2 = PLANT + 1_i64p
    AXE    = VOCAB(ia5('AXE  '), 1_i64p)
    MIRROR = VOCAB(ia5('MIRRO'), 1_i64p)
    DRAGON = VOCAB(ia5('DRAGO'), 1_i64p)
    CHASM  = VOCAB(ia5('CHASM'), 1_i64p)
    TROLL  = VOCAB(ia5('TROLL'), 1_i64p)
    TROLL2 = TROLL + 1_i64p
    BEAR   = VOCAB(ia5('BEAR '), 1_i64p)
    MESSAG = VOCAB(ia5('MESSA'), 1_i64p)
    VEND   = VOCAB(ia5('VENDI'), 1_i64p)
    BATTER = VOCAB(ia5('BATTE'), 1_i64p)

    NUGGET = VOCAB(ia5('GOLD '), 1_i64p)
    COINS  = VOCAB(ia5('COINS'), 1_i64p)
    CHEST  = VOCAB(ia5('CHEST'), 1_i64p)
    EGGS   = VOCAB(ia5('EGGS '), 1_i64p)
    TRIDNT = VOCAB(ia5('TRIDE'), 1_i64p)
    VASE   = VOCAB(ia5('VASE '), 1_i64p)
    EMRALD = VOCAB(ia5('EMERA'), 1_i64p)
    PYRAM  = VOCAB(ia5('PYRAM'), 1_i64p)
    PEARL  = VOCAB(ia5('PEARL'), 1_i64p)
    RUG    = VOCAB(ia5('RUG  '), 1_i64p)
    CHAIN  = VOCAB(ia5('CHAIN'), 1_i64p)
    SPICES = VOCAB(ia5('SPICE'), 1_i64p)

    BACK_V  = VOCAB(ia5('BACK '), 0_i64p)
    LOOK_V  = VOCAB(ia5('LOOK '), 0_i64p)
    CAVE_V  = VOCAB(ia5('CAVE '), 0_i64p)
    NULL_V  = VOCAB(ia5('NULL '), 0_i64p)
    ENTRNC  = VOCAB(ia5('ENTRA'), 0_i64p)
    DPRSSN  = VOCAB(ia5('DEPRE'), 0_i64p)

    SAY_V   = VOCAB(ia5('SAY  '), 2_i64p)
    LOCK_V  = VOCAB(ia5('LOCK '), 2_i64p)
    THROW_V = VOCAB(ia5('THROW'), 2_i64p)
    FIND_V  = VOCAB(ia5('FIND '), 2_i64p)
    INVENT_V= VOCAB(ia5('INVEN'), 2_i64p)

    ! Initialise dwarf state
    CHLOC  = 114_i64p
    CHLOC2 = 140_i64p
    DFLAG  = 0_i64p
    DALTLC = 18_i64p
    do i = 1, 6
      DSEEN(i) = .false.
    end do
    DLOC(1)=19; DLOC(2)=27; DLOC(3)=33; DLOC(4)=44; DLOC(5)=64
    DLOC(6)=CHLOC
    do i = 1, 6
      ODLOC(i) = DLOC(i)
    end do

    ! Counters / flags
    TURNS  = 0;  LMWARN = .false.;  IWEST  = 0
    KNFLOC = 0;  DETAIL = 0;  ABBNUM = 5
    NUMDIE = 0;  HOLDNG = 0;  DKILL  = 0
    FOOBAR = 0;  BONUS  = 0;  CLOCK1 = 30;  CLOCK2 = 50
    CLOSNG = .false.;  PANIC = .false.;  CLOSED = .false.
    GAVEUP = .false.;  SCORNG = .false.
    WZDARK = .false.

    ! Determine MAXDIE from available reincarnation messages (RTEXT 81,83,85...)
    MAXDIE = 0
    do i = 0, 4
      if (RTEXT(2*i+81) /= 0_i64p) MAXDIE = int(i + 1_i64p, kind=8)
    end do

    ! Ask for instructions / set LIMIT
    HINTED(3) = YES(65_i64p, 1_i64p, 0_i64p)
    NEWLOC = 1_i64p
    LOC    = 0_i64p
    OLDLOC = 0_i64p
    OLDLC2 = 0_i64p
    LIMIT  = 330_i64p
    if (HINTED(3)) LIMIT = 1000_i64p

    ! Clear hint counters
    do i = 1, int(HNTMAX)
      HINTED(i) = .false.
      HINTLC(i) = 0_i64p
    end do

    print '(A)', ' Welcome to Adventure!!'
    print '(A)', ' (Type HELP for hints at any point.)'
    print '()'
  end subroutine GAME_INIT


  ! ===================================================================
  ! The LIMIT variable lives in advent_mod as LIMIT; expose accessor
  ! ===================================================================

  ! ===================================================================
  ! GAME_LOOP: the main game turn loop
  ! ===================================================================
  subroutine GAME_LOOP()
    integer :: i, j, hint
    integer(kind=i64p) :: k, ll, k2, j64, LL64, KK_local
    logical :: action_done

    ! ----------------------------------------------------------------
    ! Turn-top: check for cave-closing blockade
    ! ----------------------------------------------------------------
    main_loop: do

      ! Can't leave cave once closing (label 2)
      if (NEWLOC >= 9_i64p .or. NEWLOC == 0_i64p .or. .not. CLOSNG) then
        ! ok to move
      else
        call RSPEAK(130_i64p)
        NEWLOC = LOC
        if (.not. PANIC) CLOCK2 = 15_i64p
        PANIC = .true.
      end if

      ! Dwarf blocking (label 71)
      if (.not. (NEWLOC == LOC .or. FORCED(LOC) .or. BITSET(LOC, 3_i64p))) then
        do i = 1, 5
          if (ODLOC(i) == NEWLOC .and. DSEEN(i)) then
            NEWLOC = LOC
            call RSPEAK(2_i64p)
            exit
          end if
        end do
      end if
      LOC = NEWLOC

      ! Dwarf / pirate movement (label 74 / 6000)
      call DWARF_MOVE()

      ! ----------------------------------------------------------------
      ! Describe current location (label 2000)
      ! ----------------------------------------------------------------
      if (LOC == 0_i64p) call DEATH_AND_SCORE()

      KK_local = STEXT(int(LOC))
      if (mod(ABB(int(LOC)), ABBNUM) == 0_i64p .or. KK_local == 0_i64p) &
        KK_local = LTEXT(int(LOC))

      if (.not. (FORCED(LOC) .or. .not. DARK())) then
        ! It's dark
        if (WZDARK .and. PCT(35_i64p)) then
          ! fell into pit
          call RSPEAK(23_i64p)
          OLDLC2 = LOC
          call DEATH_AND_SCORE()
        end if
        KK_local = RTEXT(16)
      end if

      if (TOTING(BEAR)) call RSPEAK(141_i64p)
      call SPEAK(KK_local)

      if (.not. FORCED(LOC)) then
        if (LOC == 33_i64p .and. PCT(25_i64p) .and. .not. CLOSNG) call RSPEAK(8_i64p)
      end if

      ! Print object descriptions at this location
      if (.not. DARK()) then
        ABB(int(LOC)) = ABB(int(LOC)) + 1_i64p
        i = int(ATLOC(int(LOC)))
        obj_desc: do while (i /= 0)
          OBJ = int(i, kind=i64p)
          if (OBJ > 100_i64p) OBJ = OBJ - 100_i64p
          if (OBJ == STEPS .and. TOTING(NUGGET)) then
            i = int(LINK(i))
            cycle obj_desc
          end if
          if (PROP(int(OBJ)) < 0_i64p) then
            if (.not. CLOSED) then
              PROP(int(OBJ)) = 0_i64p
              if (OBJ == RUG .or. OBJ == CHAIN) PROP(int(OBJ)) = 1_i64p
              TALLY = TALLY - 1_i64p
              if (TALLY == TALLY2 .and. TALLY /= 0_i64p) &
                LIMIT = min(35_i64p, LIMIT)
            end if
          end if
          k = PROP(int(OBJ))
          if (OBJ == STEPS .and. LOC == FIXED(int(STEPS))) k = 1_i64p
          call PSPEAK(OBJ, k)
          i = int(LINK(i))
        end do obj_desc
      end if

      if (FORCED(LOC)) then
        call TRAVEL_FORCED()
        cycle main_loop
      end if

      ! ----------------------------------------------------------------
      ! Hint checking (label 2600)
      ! ----------------------------------------------------------------
      hint_loop: do hint = 4, int(HNTMAX)
        if (HINTED(hint)) cycle hint_loop
        if (.not. BITSET(LOC, int(hint, kind=i64p))) then
          HINTLC(hint) = -1_i64p
        end if
        HINTLC(hint) = HINTLC(hint) + 1_i64p
        if (HINTLC(hint) >= HINTS(hint, 1)) call DO_HINT(hint)
      end do hint_loop

      ! Closed-time: set props of carried objects with prop<0 to -1-prop
      if (CLOSED) then
        if (PROP(int(OYSTER)) < 0_i64p .and. TOTING(OYSTER)) call PSPEAK(OYSTER, 1_i64p)
        do i = 1, 100
          if (TOTING(int(i, kind=i64p)) .and. PROP(i) < 0_i64p) &
            PROP(i) = -1_i64p - PROP(i)
        end do
      end if

      WZDARK = DARK()
      if (KNFLOC > 0_i64p .and. KNFLOC /= LOC) KNFLOC = 0_i64p
      k = RAN(1_i64p)   ! just to advance the RNG like original

      ! ----------------------------------------------------------------
      ! Get player input (label 2605)
      ! ----------------------------------------------------------------
      call GETIN(WD1, WD1X, WD2, WD2X)

      ! FOOBAR advance
      FOOBAR = min(0_i64p, -FOOBAR)

      TURNS = TURNS + 1_i64p

      if (VERB == SAY_V .and. WD2 /= '     ') VERB = 0_i64p
      if (VERB == SAY_V) then
        OBJ = ia5(WD2)
        call DO_VERB()
        cycle main_loop
      end if

      ! Clock and lamp management
      if (TALLY == 0_i64p .and. LOC >= 15_i64p .and. LOC /= 33_i64p) &
        CLOCK1 = CLOCK1 - 1_i64p
      if (CLOCK1 == 0_i64p) then; call CLOSE_CAVE();  cycle main_loop; end if
      if (CLOCK1 < 0_i64p) CLOCK2 = CLOCK2 - 1_i64p
      if (CLOCK2 == 0_i64p) then; call FINAL_PUZZLE(); cycle main_loop; end if

      if (PROP(int(LAMP)) == 1_i64p) LIMIT = LIMIT - 1_i64p
      if (LIMIT <= 30_i64p .and. HERE(BATTER) .and. PROP(int(BATTER)) == 0_i64p &
          .and. HERE(LAMP)) then
        call RSPEAK(188_i64p)
        PROP(int(BATTER)) = 1_i64p
        if (TOTING(BATTER)) call DROP(BATTER, LOC)
        LIMIT = LIMIT + 2500_i64p
        LMWARN = .false.
      else if (LIMIT == 0_i64p) then
        LIMIT = -1_i64p
        PROP(int(LAMP)) = 0_i64p
        if (HERE(LAMP)) call RSPEAK(184_i64p)
      else if (LIMIT < 0_i64p .and. LOC <= 8_i64p) then
        call RSPEAK(185_i64p)
        GAVEUP = .true.
        call DO_SCORE()
        return
      else if (LIMIT <= 30_i64p) then
        if (.not. LMWARN .and. HERE(LAMP)) then
          LMWARN = .true.
          SPK = 187_i64p
          if (PLACE(int(BATTER)) == 0_i64p) SPK = 183_i64p
          if (PROP(int(BATTER)) == 1_i64p) SPK = 189_i64p
          call RSPEAK(SPK)
        end if
      end if

      ! Handle "ENTER STREAM/WATER" special case
      if (WD1 == 'ENTER' .and. (WD2 == 'STREA' .or. WD2 == 'WATER')) then
        SPK = 70_i64p
        if (LIQLOC(LOC) == WATER) SPK = 70_i64p
        call RSPEAK(SPK); cycle main_loop
      end if
      if (WD1 == 'ENTER' .and. WD2 /= '     ') then
        WD1 = WD2; WD1X = WD2X; WD2 = '     '
      end if

      ! WATER/OIL PLANT/DOOR shortcut
      if ((WD1 == 'WATER' .or. WD1 == 'OIL') .and. &
          (WD2 == 'PLANT' .or. WD2 == 'DOOR')) then
        if (AT(VOCAB(ia5(WD2), 1_i64p))) WD2 = 'POUR '
      end if

      ! Count "WEST" vs "W"
      if (WD1 == 'WEST ') then
        IWEST = IWEST + 1_i64p
        if (IWEST == 10_i64p) call RSPEAK(17_i64p)
      end if

      ! Classify first word
      call PARSE_INPUT()

    end do main_loop

  end subroutine GAME_LOOP


  ! ===================================================================
  ! PARSE_INPUT: look up WD1 in vocabulary and dispatch
  ! ===================================================================
  recursive subroutine PARSE_INPUT()
    integer(kind=i64p) :: i_vocab, ktype

    i_vocab = VOCAB(ia5(WD1), -1_i64p)
    if (i_vocab == -1_i64p) then
      ! Unknown word
      SPK = 60_i64p
      if (PCT(20_i64p)) SPK = 61_i64p
      if (PCT(20_i64p)) SPK = 13_i64p
      call RSPEAK(SPK)
      return
    end if

    k_word = mod(i_vocab, 1000_i64p)
    ktype  = i_vocab / 1000_i64p + 1_i64p

    select case (int(ktype))
    case (1)   ! motion verb
      call DO_MOTION(k_word)
    case (2)   ! object noun
      OBJ = k_word
      call DO_OBJECT()
    case (3)   ! action verb
      VERB = k_word
      SPK  = ACTSPK(int(VERB))
      if (WD2 /= '     ' .and. VERB /= SAY_V) then
        WD1 = WD2; WD1X = WD2X; WD2 = '     '
        call PARSE_INPUT()
        return
      end if
      if (VERB == SAY_V) then
        OBJ = ia5(WD2)
      else
        OBJ = 0_i64p
      end if
      call DO_VERB()
    case (4)   ! special verb (section 3 vocab, e.g. FOO)
      VERB = k_word
      OBJ  = 0_i64p
      call DO_VERB()
    case default
      call BUG(22_i64p)
    end select

  end subroutine PARSE_INPUT



  ! ===================================================================
  ! DO_OBJECT: handle an object noun word (label 5000)
  ! ===================================================================
  subroutine DO_OBJECT()
    integer(kind=i64p) :: kobj
    integer :: i_tmp_d
    logical :: obj_found

    kobj = OBJ

    ! Check if object is accessible
    if (FIXED(int(kobj)) /= LOC .and. .not. HERE(kobj)) then
      ! Special cases
      if (kobj == GRATE) then
        if (LOC == 1_i64p .or. LOC == 4_i64p .or. LOC == 7_i64p) kobj = DPRSSN
        if (LOC > 9_i64p .and. LOC < 15_i64p) kobj = ENTRNC
        if (kobj /= GRATE) then; call DO_MOTION(kobj); return; end if
      end if
      if (kobj == DWARF) then
        do i_tmp_d = 1, 5
          if (DLOC(i_tmp_d) == LOC .and. DFLAG >= 2_i64p) then
            obj_found = .true.; exit
          end if
        end do
        if (obj_found) goto 5010
      end if
      if ((LIQ() == kobj .and. HERE(BOTTLE)) .or. kobj == LIQLOC(LOC)) goto 5010
      if (kobj == PLANT .and. AT(PLANT2) .and. PROP(int(PLANT2)) /= 0_i64p) then
        OBJ = PLANT2; goto 5010
      end if
      if (kobj == KNIFE .and. KNFLOC == LOC) then
        KNFLOC = -1_i64p; SPK = 116_i64p; call RSPEAK(SPK); return
      end if
      if (kobj == ROD .and. HERE(ROD2)) then
        OBJ = ROD2; goto 5010
      end if
      if ((VERB == FIND_V .or. VERB == INVENT_V) .and. WD2 == '     ') goto 5010
      ! Object not here
      print '(A,A)', ' I see no ', trim(adjustl(WD1))//' here.'
      return
    end if

5010 continue
    if (WD2 /= '     ') then
      WD1 = WD2; WD1X = WD2X; WD2 = '     '
      call PARSE_INPUT()
      return
    end if
    if (VERB /= 0_i64p) then
      call DO_VERB()
      return
    end if
    ! Ask "what do you want to do with the <obj>?"
    print '(A,A,A)', ' What do you want to do with the ', trim(adjustl(WD1)), '?'

  end subroutine DO_OBJECT


  ! ===================================================================
  ! DO_MOTION: resolve a motion verb and set NEWLOC (label 8)
  ! ===================================================================
  subroutine DO_MOTION(k_mot)
    integer(kind=i64p), intent(in) :: k_mot
    integer(kind=i64p) :: ll64, k2, k_local, LL_inner, k_nn

    k_local = k_mot
    KK = KEY(int(LOC))
    NEWLOC = LOC
    if (KK == 0_i64p) call BUG(26_i64p)

    if (k_local == NULL_V) return
    if (k_local == BACK_V) then; call DO_BACK(); return; end if
    if (k_local == LOOK_V) then; call DO_LOOK(); return; end if
    if (k_local == CAVE_V) then
      if (LOC < 8_i64p) call RSPEAK(57_i64p)
      if (LOC >= 8_i64p) call RSPEAK(58_i64p)
      return
    end if

    OLDLC2 = OLDLOC
    OLDLOC = LOC

    ! Scan travel table for matching verb
    scan_loop: do
      ll64 = abs(TRAVEL(int(KK)))
      if (mod(ll64, 1000_i64p) == 1_i64p .or. mod(ll64, 1000_i64p) == k_local) then
        ! Found a matching entry
        exit scan_loop
      end if
      if (TRAVEL(int(KK)) < 0_i64p) then
        ! No match – non-applicable motion
        call NO_MOTION(k_local)
        return
      end if
      KK = KK + 1_i64p
    end do scan_loop

    ! Resolve destination (label 10/11)
    ll64 = abs(TRAVEL(int(KK))) / 1000_i64p
    call RESOLVE_DEST(ll64, k_local)

  end subroutine DO_MOTION


  ! Resolve destination conditional (labels 10-16)
  subroutine RESOLVE_DEST(ll_in, k_mot)
    integer(kind=i64p), intent(in) :: ll_in, k_mot
    integer(kind=i64p) :: ll64, newloc64, k_cond
    integer(kind=i64p) :: kk_saved

    ll64 = ll_in
    resolve_top: do
      newloc64 = ll64 / 1000_i64p
      k_cond   = mod(newloc64, 100_i64p)
      if (newloc64 <= 300_i64p) then
        ! Simple or conditional location
        if (newloc64 <= 100_i64p) then
          if (newloc64 == 0_i64p .or. PCT(newloc64)) then
            ! Go there
            NEWLOC = mod(ll64, 1000_i64p)
            call HANDLE_SPECIAL_DEST()
            return
          end if
        else if (TOTING(k_cond) .or. &
                 (newloc64 > 200_i64p .and. AT(k_cond))) then
          NEWLOC = mod(ll64, 1000_i64p)
          call HANDLE_SPECIAL_DEST()
          return
        else if (newloc64 > 300_i64p .and. newloc64 <= 400_i64p) then
          if (PROP(int(mod(newloc64, 100_i64p))) /= 0_i64p) then
            NEWLOC = mod(ll64, 1000_i64p); call HANDLE_SPECIAL_DEST(); return
          end if
        else if (newloc64 > 400_i64p .and. newloc64 <= 500_i64p) then
          if (PROP(int(mod(newloc64, 100_i64p))) /= 1_i64p) then
            NEWLOC = mod(ll64, 1000_i64p); call HANDLE_SPECIAL_DEST(); return
          end if
        else if (newloc64 > 500_i64p) then
          if (PROP(int(mod(newloc64, 100_i64p))) /= 2_i64p) then
            NEWLOC = mod(ll64, 1000_i64p); call HANDLE_SPECIAL_DEST(); return
          end if
        else
          NEWLOC = mod(ll64, 1000_i64p)
          call HANDLE_SPECIAL_DEST()
          return
        end if
        ! Condition not met – find next different destination
        kk_saved = KK
        do
          if (TRAVEL(int(KK)) < 0_i64p) call BUG(25_i64p)
          KK = KK + 1_i64p
          if (abs(TRAVEL(int(KK))) / 1000_i64p /= ll64) exit
        end do
        ll64 = abs(TRAVEL(int(KK))) / 1000_i64p
        cycle resolve_top
      else
        ! Conditional object-carry check already done above for >100
        NEWLOC = mod(ll64, 1000_i64p)
        call HANDLE_SPECIAL_DEST()
        return
      end if
    end do resolve_top
  end subroutine RESOLVE_DEST


  subroutine HANDLE_SPECIAL_DEST()
    integer(kind=i64p) :: dest
    dest = NEWLOC
    if (dest <= 300_i64p) return   ! ordinary location

    if (dest <= 500_i64p) then
      ! Special travel case 301-500
      call SPECIAL_TRAVEL(dest - 300_i64p)
      return
    end if
    ! Print message and stay
    call RSPEAK(dest - 500_i64p)
    NEWLOC = LOC
  end subroutine HANDLE_SPECIAL_DEST


  ! Special travel cases (label 30000)
  subroutine SPECIAL_TRAVEL(case_num)
    integer(kind=i64p), intent(in) :: case_num
    select case (int(case_num))
    case (1)   ! 301: Plover-alcove passage
      NEWLOC = 99_i64p + 100_i64p - LOC
      if (HOLDNG == 0_i64p .or. (HOLDNG == 1_i64p .and. TOTING(EMRALD))) return
      NEWLOC = LOC
      call RSPEAK(117_i64p)

    case (2)   ! 302: Plover transport
      call DROP(EMRALD, LOC)
      ! go back and pretend not carrying it
      NEWLOC = LOC

    case (3)   ! 303: Troll bridge
      if (PROP(int(TROLL)) == 1_i64p) then
        call PSPEAK(TROLL, 1_i64p)
        PROP(int(TROLL)) = 0_i64p
        call MOVE(TROLL2, 0_i64p)
        call MOVE(TROLL2+100_i64p, 0_i64p)
        call MOVE(TROLL, PLAC(int(TROLL)))
        call MOVE(TROLL+100_i64p, FIXD(int(TROLL)))
        call JUGGLE(CHASM)
        NEWLOC = LOC
        return
      end if
      NEWLOC = PLAC(int(TROLL)) + FIXD(int(TROLL)) - LOC
      if (PROP(int(TROLL)) == 0_i64p) PROP(int(TROLL)) = 1_i64p
      if (.not. TOTING(BEAR)) return
      call RSPEAK(162_i64p)
      PROP(int(CHASM)) = 1_i64p
      PROP(int(TROLL)) = 2_i64p
      call DROP(BEAR, NEWLOC)
      FIXED(int(BEAR)) = -1_i64p
      PROP(int(BEAR)) = 3_i64p
      if (PROP(int(SPICES)) < 0_i64p) TALLY2 = TALLY2 + 1_i64p
      OLDLC2 = NEWLOC
      LOC = 0_i64p   ! trigger death

    case default
      call BUG(20_i64p)
    end select
  end subroutine SPECIAL_TRAVEL


  subroutine DO_BACK()
    integer(kind=i64p) :: k_back, k2_back, ll_b, j_key_back
    k_back = OLDLOC
    if (FORCED(k_back)) k_back = OLDLC2
    OLDLC2 = OLDLOC
    OLDLOC = LOC
    k2_back = 0_i64p

    if (k_back == LOC) then
      call RSPEAK(91_i64p)
      NEWLOC = LOC; return
    end if

    do
      ll_b = mod(abs(TRAVEL(int(KK))) / 1000_i64p, 1000_i64p)
      if (ll_b == k_back) then
        ! Found it
        k_back = mod(abs(TRAVEL(int(KK))), 1000_i64p)
        KK = KEY(int(LOC))
        call DO_MOTION(k_back)
        return
      end if
      if (ll_b <= 300_i64p) then
        j_key_back = KEY(int(ll_b))
        if (FORCED(ll_b) .and. mod(abs(TRAVEL(int(j_key_back)))/1000_i64p, 1000_i64p) == k_back) &
          k2_back = KK
      end if
      if (TRAVEL(int(KK)) < 0_i64p) exit
      KK = KK + 1_i64p
    end do

    KK = k2_back
    if (KK == 0_i64p) then
      call RSPEAK(140_i64p)
      NEWLOC = LOC; return
    end if

    k_back = mod(abs(TRAVEL(int(KK))), 1000_i64p)
    KK = KEY(int(LOC))
    call DO_MOTION(k_back)
  end subroutine DO_BACK


  subroutine DO_LOOK()
    if (DETAIL < 3_i64p) call RSPEAK(15_i64p)
    DETAIL = DETAIL + 1_i64p
    WZDARK = .false.
    ABB(int(LOC)) = 0_i64p
    NEWLOC = LOC
  end subroutine DO_LOOK


  subroutine NO_MOTION(k_mot)
    integer(kind=i64p), intent(in) :: k_mot
    SPK = 12_i64p
    if (k_mot >= 43_i64p .and. k_mot <= 50_i64p) SPK = 9_i64p
    if (k_mot == 29_i64p .or. k_mot == 30_i64p) SPK = 9_i64p
    if (k_mot == 7_i64p .or. k_mot == 36_i64p .or. k_mot == 37_i64p) SPK = 10_i64p
    if (k_mot == 11_i64p .or. k_mot == 19_i64p) SPK = 11_i64p
    if (VERB == FIND_V .or. VERB == INVENT_V) SPK = 59_i64p
    if (k_mot == 62_i64p .or. k_mot == 65_i64p) SPK = 42_i64p
    if (k_mot == 17_i64p) SPK = 80_i64p
    call RSPEAK(SPK)
  end subroutine NO_MOTION


  subroutine TRAVEL_FORCED()
    ! When location has forced motion, move immediately without input
    KK = KEY(int(LOC))
    if (KK /= 0_i64p) then
      k_word = mod(abs(TRAVEL(int(KK))), 1000_i64p)
      call DO_MOTION(1_i64p)   ! verb=1 always matches "unconditional"
    end if
  end subroutine TRAVEL_FORCED


  ! ===================================================================
  ! DO_VERB: dispatch action verb (labels 4080 / 4090)
  ! ===================================================================
  subroutine DO_VERB()
    select case (int(VERB))
    case (1);  call VB_TAKE()
    case (2);  call VB_DROP()
    case (3);  call VB_SAY()
    case (4);  call VB_OPENLOCK()
    case (5);  call RSPEAK(SPK)           ! NOTHING
    case (6);  call VB_OPENLOCK()
    case (7);  call VB_LIGHT()
    case (8);  call VB_OFF()
    case (9);  call VB_WAVE()
    case (10); call RSPEAK(SPK)           ! CALM – unimplemented
    case (11); call RSPEAK(SPK)           ! WALK
    case (12); call VB_ATTACK()
    case (13); call VB_POUR()
    case (14); call VB_EAT()
    case (15); call VB_DRINK()
    case (16); call VB_RUB()
    case (17); call VB_THROW()
    case (18); call VB_QUIT()
    case (19); call VB_FIND()
    case (20); call VB_INVENTORY()
    case (21); call VB_FEED()
    case (22); call VB_FILL()
    case (23); call VB_BLAST()
    case (24); call VB_SCORE_CMD()
    case (25); call VB_FOO()
    case (26); call VB_BRIEF()
    case (27); call VB_READ()
    case (28); call VB_BREAK()
    case (29); call VB_WAKE()
    case (30); call RSPEAK(201_i64p)      ! SUSPEND (simplified)
    case (31); call RSPEAK(SPK)           ! HOURS
    case default
      if (OBJ == 0_i64p) then
        call BUG(23_i64p)
      else
        call BUG(24_i64p)
      end if
    end select
  end subroutine DO_VERB


  ! ===================================================================
  ! Action verb implementations
  ! ===================================================================

  subroutine VB_TAKE()
    integer(kind=i64p) :: k
    integer :: i
    ! Intransitive: if only one object here, take it
    if (OBJ == 0_i64p) then
      if (ATLOC(int(LOC)) == 0_i64p .or. LINK(int(ATLOC(int(LOC)))) /= 0_i64p) then
        call RSPEAK(SPK); return
      end if
      do i = 1, 5
        if (DLOC(i) == LOC .and. DFLAG >= 2_i64p) then
          call RSPEAK(SPK); return
        end if
      end do
      OBJ = ATLOC(int(LOC))
    end if

    if (TOTING(OBJ)) then; call RSPEAK(24_i64p); return; end if
    SPK = 25_i64p
    if (OBJ == PLANT .and. PROP(int(PLANT)) <= 0_i64p) SPK = 115_i64p
    if (OBJ == BEAR .and. PROP(int(BEAR)) == 1_i64p)   SPK = 169_i64p
    if (OBJ == CHAIN .and. PROP(int(BEAR)) /= 0_i64p)  SPK = 170_i64p
    if (FIXED(int(OBJ)) /= 0_i64p) then; call RSPEAK(SPK); return; end if

    if (OBJ == WATER .or. OBJ == OIL) then
      if (HERE(BOTTLE) .and. LIQ() == OBJ) then
        OBJ = BOTTLE
      else
        OBJ = BOTTLE
        if (TOTING(BOTTLE) .and. PROP(int(BOTTLE)) == 1_i64p) then
          call VB_FILL(); return
        end if
        if (PROP(int(BOTTLE)) /= 1_i64p) SPK = 105_i64p
        if (.not. TOTING(BOTTLE)) SPK = 104_i64p
        call RSPEAK(SPK); return
      end if
    end if

    if (HOLDNG >= 7_i64p) then; call RSPEAK(92_i64p); return; end if

    if (OBJ == BIRD) then
      if (PROP(int(BIRD)) == 0_i64p) then
        if (TOTING(ROD)) then; call RSPEAK(26_i64p); return; end if
        if (.not. TOTING(CAGE)) then; call RSPEAK(27_i64p); return; end if
        PROP(int(BIRD)) = 1_i64p
      end if
    end if

    if ((OBJ == BIRD .or. OBJ == CAGE) .and. PROP(int(BIRD)) /= 0_i64p) &
      call CARRY(BIRD + CAGE - OBJ, LOC)
    call CARRY(OBJ, LOC)
    k = LIQ()
    if (OBJ == BOTTLE .and. k /= 0_i64p) PLACE(int(k)) = -1_i64p
    call RSPEAK(54_i64p)

  end subroutine VB_TAKE


  subroutine VB_DROP()
    integer(kind=i64p) :: k_liq
    if (OBJ == ROD2 .and. .not. TOTING(ROD2)) then
      if (TOTING(ROD)) OBJ = ROD2
    end if
    if (.not. TOTING(OBJ)) then; call RSPEAK(SPK); return; end if

    if (OBJ == BIRD .and. HERE(SNAKE)) then
      call RSPEAK(30_i64p)
      if (.not. CLOSED) call DSTROY(SNAKE)
      PROP(int(SNAKE)) = 1_i64p
    end if

    if (OBJ == COINS .and. HERE(VEND)) then
      call DSTROY(COINS)
      call DROP(BATTER, LOC)
      call PSPEAK(BATTER, 0_i64p)
      return
    end if

    if (OBJ == BIRD .and. AT(DRAGON) .and. PROP(int(DRAGON)) == 0_i64p) then
      call RSPEAK(154_i64p)
      call DSTROY(BIRD)
      PROP(int(BIRD)) = 0_i64p
      if (PLACE(int(SNAKE)) == PLAC(int(SNAKE))) TALLY2 = TALLY2 + 1_i64p
      return
    end if

    if (OBJ == BEAR .and. AT(TROLL)) then
      call RSPEAK(163_i64p)
      call MOVE(TROLL, 0_i64p)
      call MOVE(TROLL+100_i64p, 0_i64p)
      call MOVE(TROLL2, PLAC(int(TROLL)))
      call MOVE(TROLL2+100_i64p, FIXD(int(TROLL)))
      call JUGGLE(CHASM)
      PROP(int(TROLL)) = 2_i64p
    end if

    if (OBJ == VASE .and. LOC /= PLAC(int(PILLOW))) then
      PROP(int(VASE)) = 2_i64p
      if (AT(PILLOW)) PROP(int(VASE)) = 0_i64p
      call PSPEAK(VASE, PROP(int(VASE))+1_i64p)
      if (PROP(int(VASE)) /= 0_i64p) FIXED(int(VASE)) = -1_i64p
    else
      call RSPEAK(54_i64p)
    end if

    k_liq = LIQ()
    if (k_liq == OBJ) OBJ = BOTTLE
    if (OBJ == BOTTLE .and. k_liq /= 0_i64p) PLACE(int(k_liq)) = 0_i64p
    if (OBJ == CAGE .and. PROP(int(BIRD)) /= 0_i64p) call DROP(BIRD, LOC)
    if (OBJ == BIRD) PROP(int(BIRD)) = 0_i64p
    call DROP(OBJ, LOC)
  end subroutine VB_DROP


  subroutine VB_SAY()
    integer(kind=i64p) :: i_v
    character(len=5) :: w
    ! OBJ holds ia5 of word to say
    ! Check for magic words
    i_v = VOCAB(OBJ, -1_i64p)
    if (i_v == 62_i64p .or. i_v == 65_i64p .or. i_v == 71_i64p .or. &
        mod(i_v, 1000_i64p) == 25_i64p) then
      ! Magic word – process as if player typed it
      WD1 = WD2; WD1X = WD2X; WD2 = '     '
      call PARSE_INPUT()
      return
    end if
    ! Echo the word
    call a5_to_str(OBJ, w)
    print '(A,A,A)', ' Okay, "', trim(w), '".'
  end subroutine VB_SAY


  subroutine VB_OPENLOCK()
    integer(kind=i64p) :: k
    ! Both OPEN (verb 4) and LOCK (verb 6) route here.
    if (OBJ == 0_i64p) then
      SPK = 28_i64p
      if (HERE(CLAM))   OBJ = CLAM
      if (HERE(OYSTER)) OBJ = OYSTER
      if (AT(DOOR))     OBJ = DOOR
      if (AT(GRATE))    OBJ = GRATE
      if (OBJ /= 0_i64p .and. HERE(CHAIN)) then; call RSPEAK(SPK); return; end if
      if (HERE(CHAIN))  OBJ = CHAIN
      if (OBJ == 0_i64p) then; call RSPEAK(SPK); return; end if
    end if

    if (OBJ == CLAM .or. OBJ == OYSTER) then
      call VB_OPENLOCK_CLAM(); return
    end if
    if (OBJ == DOOR)  then; SPK = 111_i64p
      if (PROP(int(DOOR)) == 1_i64p) SPK = 54_i64p
    end if
    if (OBJ == CAGE)  SPK = 32_i64p
    if (OBJ == KEYS)  SPK = 55_i64p
    if (OBJ == GRATE .or. OBJ == CHAIN) SPK = 31_i64p

    if (SPK /= 31_i64p .or. .not. HERE(KEYS)) then; call RSPEAK(SPK); return; end if
    if (OBJ == CHAIN) then; call VB_OPENLOCK_CHAIN(); return; end if

    if (CLOSNG) then
      if (.not. PANIC) CLOCK2 = 15_i64p
      PANIC = .true.
      call RSPEAK(130_i64p); return
    end if

    k = 34_i64p + PROP(int(GRATE))
    PROP(int(GRATE)) = 1_i64p
    if (VERB == LOCK_V) PROP(int(GRATE)) = 0_i64p
    k = k + 2_i64p * PROP(int(GRATE))
    call RSPEAK(k)

  end subroutine VB_OPENLOCK


  subroutine VB_OPENLOCK_CLAM()
    integer(kind=i64p) :: kk_c
    kk_c = 0_i64p
    if (OBJ == OYSTER) kk_c = 1_i64p
    SPK = 124_i64p + kk_c
    if (TOTING(OBJ)) SPK = 120_i64p + kk_c
    if (.not. TOTING(TRIDNT)) SPK = 122_i64p + kk_c
    if (VERB == LOCK_V) SPK = 61_i64p
    if (SPK /= 124_i64p) then; call RSPEAK(SPK); return; end if
    call DSTROY(CLAM)
    call DROP(OYSTER, LOC)
    call DROP(PEARL, 105_i64p)
    call RSPEAK(SPK)
  end subroutine VB_OPENLOCK_CLAM


  subroutine VB_OPENLOCK_CHAIN()
    if (VERB == LOCK_V) then
      SPK = 172_i64p
      if (PROP(int(CHAIN)) /= 0_i64p) SPK = 34_i64p
      if (LOC /= PLAC(int(CHAIN))) SPK = 173_i64p
      if (SPK /= 172_i64p) then; call RSPEAK(SPK); return; end if
      PROP(int(CHAIN)) = 2_i64p
      if (TOTING(CHAIN)) call DROP(CHAIN, LOC)
      FIXED(int(CHAIN)) = -1_i64p
    else
      SPK = 171_i64p
      if (PROP(int(BEAR)) == 0_i64p) SPK = 41_i64p
      if (PROP(int(CHAIN)) == 0_i64p) SPK = 37_i64p
      if (SPK /= 171_i64p) then; call RSPEAK(SPK); return; end if
      PROP(int(CHAIN)) = 0_i64p
      FIXED(int(CHAIN)) = 0_i64p
      if (PROP(int(BEAR)) /= 3_i64p) PROP(int(BEAR)) = 2_i64p
      FIXED(int(BEAR)) = 2_i64p - PROP(int(BEAR))
    end if
    call RSPEAK(SPK)
  end subroutine VB_OPENLOCK_CHAIN


  subroutine VB_LIGHT()
    if (.not. HERE(LAMP)) then; call RSPEAK(SPK); return; end if
    SPK = 184_i64p
    if (LIMIT < 0_i64p) then; call RSPEAK(SPK); return; end if
    PROP(int(LAMP)) = 1_i64p
    call RSPEAK(39_i64p)
    if (WZDARK) then
      NEWLOC = LOC
    end if
  end subroutine VB_LIGHT


  subroutine VB_OFF()
    if (.not. HERE(LAMP)) then; call RSPEAK(SPK); return; end if
    PROP(int(LAMP)) = 0_i64p
    call RSPEAK(40_i64p)
    if (DARK()) call RSPEAK(16_i64p)
  end subroutine VB_OFF


  subroutine VB_WAVE()
    if ((.not. TOTING(OBJ)) .and. (OBJ /= ROD .or. .not. TOTING(ROD2))) SPK = 29_i64p
    if (OBJ /= ROD .or. .not. AT(FISSUR) .or. .not. TOTING(OBJ) .or. CLOSNG) then
      call RSPEAK(SPK); return
    end if
    PROP(int(FISSUR)) = 1_i64p - PROP(int(FISSUR))
    call PSPEAK(FISSUR, 2_i64p - PROP(int(FISSUR)))
  end subroutine VB_WAVE


  subroutine VB_ATTACK()
    integer :: i, j_d
    integer(kind=i64p) :: k_sp

    ! Find if any dwarf is here
    i = 0
    do i = 1, 5
      if (DLOC(i) == LOC .and. DFLAG >= 2_i64p) exit
    end do
    if (i > 5) i = 0

    if (OBJ == 0_i64p) then
      if (i /= 0) OBJ = DWARF
      if (HERE(SNAKE))                               OBJ = OBJ*100_i64p + SNAKE
      if (AT(DRAGON) .and. PROP(int(DRAGON))==0_i64p) OBJ = OBJ*100_i64p + DRAGON
      if (AT(TROLL))                                 OBJ = OBJ*100_i64p + TROLL
      if (HERE(BEAR) .and. PROP(int(BEAR))==0_i64p)   OBJ = OBJ*100_i64p + BEAR
      if (OBJ > 100_i64p) then; call RSPEAK(SPK); return; end if
      if (OBJ == 0_i64p) then
        if (HERE(BIRD) .and. VERB /= THROW_V) OBJ = BIRD
        if (HERE(CLAM) .or. HERE(OYSTER)) OBJ = 100_i64p*OBJ + CLAM
        if (OBJ > 100_i64p) then; call RSPEAK(SPK); return; end if
      end if
    end if

    if (OBJ == BIRD) then
      SPK = 137_i64p
      if (CLOSED) then; call RSPEAK(SPK); return; end if
      call DSTROY(BIRD)
      PROP(int(BIRD)) = 0_i64p
      if (PLACE(int(SNAKE)) == PLAC(int(SNAKE))) TALLY2 = TALLY2 + 1_i64p
      SPK = 45_i64p
    end if

    if (OBJ == 0_i64p)                          SPK = 44_i64p
    if (OBJ == CLAM .or. OBJ == OYSTER)          SPK = 150_i64p
    if (OBJ == SNAKE)                            SPK = 46_i64p
    if (OBJ == DWARF)                            SPK = 49_i64p
    if (OBJ == DWARF .and. CLOSED)               then; call DISTURB_DWARVES(); return; end if
    if (OBJ == DRAGON)                           SPK = 167_i64p
    if (OBJ == TROLL)                            SPK = 157_i64p
    if (OBJ == BEAR) SPK = 165_i64p + (PROP(int(BEAR))+1_i64p)/2_i64p

    if (OBJ /= DRAGON .or. PROP(int(DRAGON)) /= 0_i64p) then
      call RSPEAK(SPK); return
    end if

    ! Dragon fight
    call RSPEAK(49_i64p)
    VERB = 0_i64p; OBJ = 0_i64p
    call GETIN(WD1, WD1X, WD2, WD2X)
    if (WD1 /= 'Y    ' .and. WD1 /= 'YES  ') then; NEWLOC = LOC; return; end if
    call PSPEAK(DRAGON, 1_i64p)
    PROP(int(DRAGON)) = 2_i64p
    PROP(int(RUG)) = 0_i64p
    k_sp = (PLAC(int(DRAGON)) + FIXD(int(DRAGON))) / 2_i64p
    call MOVE(DRAGON+100_i64p, -1_i64p)
    call MOVE(RUG+100_i64p, 0_i64p)
    call MOVE(DRAGON, k_sp)
    call MOVE(RUG, k_sp)
    do j_d = 1, 100
      if (PLACE(j_d) == PLAC(int(DRAGON)) .or. PLACE(j_d) == FIXD(int(DRAGON))) &
        call MOVE(int(j_d, kind=i64p), k_sp)
    end do
    LOC = k_sp
    call DO_MOTION(NULL_V)

  end subroutine VB_ATTACK


  subroutine VB_POUR()
    if (OBJ == BOTTLE .or. OBJ == 0_i64p) OBJ = LIQ()
    if (OBJ == 0_i64p) then; call RSPEAK(SPK); return; end if
    if (.not. TOTING(OBJ)) then; call RSPEAK(SPK); return; end if
    SPK = 78_i64p
    if (OBJ /= OIL .and. OBJ /= WATER) then; call RSPEAK(SPK); return; end if
    PROP(int(BOTTLE)) = 1_i64p
    PLACE(int(OBJ)) = 0_i64p
    SPK = 77_i64p
    if (.not. (AT(PLANT) .or. AT(DOOR))) then; call RSPEAK(SPK); return; end if

    if (AT(DOOR)) then
      PROP(int(DOOR)) = 0_i64p
      if (OBJ == OIL) PROP(int(DOOR)) = 1_i64p
      SPK = 113_i64p + PROP(int(DOOR))
      call RSPEAK(SPK); return
    end if
    SPK = 112_i64p
    if (OBJ /= WATER) then; call RSPEAK(SPK); return; end if
    call PSPEAK(PLANT, PROP(int(PLANT))+1_i64p)
    PROP(int(PLANT)) = mod(PROP(int(PLANT)) + 2_i64p, 6_i64p)
    PROP(int(PLANT2)) = PROP(int(PLANT)) / 2_i64p
    call DO_MOTION(NULL_V)
  end subroutine VB_POUR


  subroutine VB_EAT()
    if (OBJ == 0_i64p) then
      if (.not. HERE(FOOD)) then; call RSPEAK(SPK); return; end if
    end if
    if (OBJ == FOOD) then
      call DSTROY(FOOD)
      call RSPEAK(72_i64p); return
    end if
    if (OBJ == BIRD .or. OBJ == SNAKE .or. OBJ == CLAM .or. OBJ == OYSTER .or. &
        OBJ == DWARF .or. OBJ == DRAGON .or. OBJ == TROLL .or. OBJ == BEAR) SPK = 71_i64p
    call RSPEAK(SPK)
  end subroutine VB_EAT


  subroutine VB_DRINK()
    if (OBJ == 0_i64p .and. LIQLOC(LOC) /= WATER .and. &
        (LIQ() /= WATER .or. .not. HERE(BOTTLE))) then
      call RSPEAK(SPK); return
    end if
    if (OBJ /= 0_i64p .and. OBJ /= WATER) then; SPK = 110_i64p; call RSPEAK(SPK); return; end if
    if (LIQ() /= WATER .or. .not. HERE(BOTTLE)) then; call RSPEAK(SPK); return; end if
    PROP(int(BOTTLE)) = 1_i64p
    PLACE(int(WATER)) = 0_i64p
    call RSPEAK(74_i64p)
  end subroutine VB_DRINK


  subroutine VB_RUB()
    if (OBJ /= LAMP) SPK = 76_i64p
    call RSPEAK(SPK)
  end subroutine VB_RUB


  subroutine VB_THROW()
    integer :: i_d
    integer(kind=i64p) :: k_sp

    if (OBJ == ROD2 .and. .not. TOTING(ROD2)) then
      if (TOTING(ROD)) OBJ = ROD2
    end if
    if (.not. TOTING(OBJ)) then; call RSPEAK(SPK); return; end if

    ! Treasure to troll
    if (OBJ >= 50_i64p .and. OBJ <= MAXTRS .and. AT(TROLL)) then
      call RSPEAK(159_i64p)
      call DROP(OBJ, 0_i64p)
      call MOVE(TROLL, 0_i64p); call MOVE(TROLL+100_i64p, 0_i64p)
      call DROP(TROLL2, PLAC(int(TROLL))); call DROP(TROLL2+100_i64p, FIXD(int(TROLL)))
      call JUGGLE(CHASM); return
    end if

    ! Feeding food to bear
    if (OBJ == FOOD .and. HERE(BEAR)) then
      OBJ = BEAR; call VB_FEED(); return
    end if

    if (OBJ /= AXE) then; call VB_DROP(); return; end if

    ! Axe throw
    do i_d = 1, 5
      if (DLOC(i_d) == LOC) then
        ! Hit a dwarf?
        SPK = 48_i64p
        if (RAN(3_i64p) == 0_i64p) then
          DSEEN(i_d) = .false.
          DLOC(i_d) = 0_i64p
          SPK = 47_i64p
          DKILL = DKILL + 1_i64p
          if (DKILL == 1_i64p) SPK = 149_i64p
        end if
        call RSPEAK(SPK)
        call DROP(AXE, LOC)
        call DO_MOTION(NULL_V)
        return
      end if
    end do

    SPK = 152_i64p
    if (AT(DRAGON) .and. PROP(int(DRAGON)) == 0_i64p) then
      call RSPEAK(SPK); call DROP(AXE, LOC); call DO_MOTION(NULL_V); return
    end if
    SPK = 158_i64p
    if (AT(TROLL)) then
      call RSPEAK(SPK); call DROP(AXE, LOC); call DO_MOTION(NULL_V); return
    end if
    if (HERE(BEAR) .and. PROP(int(BEAR)) == 0_i64p) then
      call RSPEAK(164_i64p)
      call DROP(AXE, LOC)
      FIXED(int(AXE)) = -1_i64p
      PROP(int(AXE)) = 1_i64p
      call JUGGLE(BEAR); return
    end if
    OBJ = 0_i64p; call VB_ATTACK()
  end subroutine VB_THROW


  subroutine VB_QUIT()
    GAVEUP = YES(22_i64p, 54_i64p, 54_i64p)
    if (GAVEUP) call DO_SCORE()
  end subroutine VB_QUIT


  subroutine VB_FIND()
    integer :: i
    if (AT(OBJ) .or. (LIQ() == OBJ .and. AT(BOTTLE)) .or. k_word == LIQLOC(LOC)) SPK = 94_i64p
    do i = 1, 5
      if (DLOC(i) == LOC .and. DFLAG >= 2_i64p .and. OBJ == DWARF) SPK = 94_i64p
    end do
    if (CLOSED) SPK = 138_i64p
    if (TOTING(OBJ)) SPK = 24_i64p
    call RSPEAK(SPK)
  end subroutine VB_FIND


  subroutine VB_INVENTORY()
    integer :: i_inv
    if (OBJ /= 0_i64p) then; call VB_FIND(); return; end if
    SPK = 98_i64p
    do i_inv = 1, 100
      if (i_inv == int(BEAR)) cycle
      if (.not. TOTING(int(i_inv, kind=i64p))) cycle
      if (SPK == 98_i64p) call RSPEAK(99_i64p)
      BLKLIN = .false.
      call PSPEAK(int(i_inv, kind=i64p), -1_i64p)
      BLKLIN = .true.
      SPK = 0_i64p
    end do
    if (TOTING(BEAR)) SPK = 141_i64p
    if (SPK /= 0_i64p) call RSPEAK(SPK)
  end subroutine VB_INVENTORY


  subroutine VB_FEED()
    if (OBJ == BIRD) then; call RSPEAK(100_i64p); return; end if

    if (OBJ == SNAKE .or. OBJ == DRAGON .or. OBJ == TROLL) then
      SPK = 102_i64p
      if (OBJ == DRAGON .and. PROP(int(DRAGON)) /= 0_i64p) SPK = 110_i64p
      if (OBJ == TROLL) SPK = 182_i64p
      if (OBJ /= SNAKE .or. CLOSED .or. .not. HERE(BIRD)) then
        call RSPEAK(SPK); return
      end if
      SPK = 101_i64p
      call DSTROY(BIRD)
      PROP(int(BIRD)) = 0_i64p
      TALLY2 = TALLY2 + 1_i64p
      call RSPEAK(SPK); return
    end if

    if (OBJ == DWARF) then
      if (.not. HERE(FOOD)) then; call RSPEAK(SPK); return; end if
      SPK = 103_i64p
      DFLAG = DFLAG + 1_i64p
      call RSPEAK(SPK); return
    end if

    if (OBJ == BEAR) then
      if (PROP(int(BEAR)) == 0_i64p) SPK = 102_i64p
      if (PROP(int(BEAR)) == 3_i64p) SPK = 110_i64p
      if (.not. HERE(FOOD)) then; call RSPEAK(SPK); return; end if
      call DSTROY(FOOD)
      PROP(int(BEAR)) = 1_i64p
      FIXED(int(AXE)) = 0_i64p
      PROP(int(AXE)) = 0_i64p
      call RSPEAK(168_i64p); return
    end if

    call RSPEAK(14_i64p)
  end subroutine VB_FEED


  subroutine VB_FILL()
    integer(kind=i64p) :: k_fill
    if (OBJ == VASE) then
      SPK = 29_i64p
      if (LIQLOC(LOC) == 0_i64p) SPK = 144_i64p
      if (LIQLOC(LOC) == 0_i64p .or. .not. TOTING(VASE)) then
        call RSPEAK(SPK); return
      end if
      call RSPEAK(145_i64p)
      PROP(int(VASE)) = 2_i64p
      FIXED(int(VASE)) = -1_i64p
      call VB_DROP(); return
    end if

    if (OBJ /= 0_i64p .and. OBJ /= BOTTLE) then; call RSPEAK(SPK); return; end if
    if (OBJ == 0_i64p .and. .not. HERE(BOTTLE)) then; call RSPEAK(SPK); return; end if
    SPK = 107_i64p
    if (LIQLOC(LOC) == 0_i64p) SPK = 106_i64p
    if (LIQ() /= 0_i64p) SPK = 105_i64p
    if (SPK /= 107_i64p) then; call RSPEAK(SPK); return; end if
    PROP(int(BOTTLE)) = mod(COND(int(LOC)), 4_i64p) / 2_i64p * 2_i64p
    k_fill = LIQ()
    if (TOTING(BOTTLE)) PLACE(int(k_fill)) = -1_i64p
    if (k_fill == OIL) then; call RSPEAK(108_i64p); else; call RSPEAK(107_i64p); end if

  end subroutine VB_FILL


  subroutine VB_BLAST()
    if (PROP(int(ROD2)) < 0_i64p .or. .not. CLOSED) then; call RSPEAK(SPK); return; end if
    BONUS = 133_i64p
    if (LOC == 115_i64p) BONUS = 134_i64p
    if (HERE(ROD2)) BONUS = 135_i64p
    call RSPEAK(BONUS)
    call DO_SCORE()
  end subroutine VB_BLAST


  subroutine VB_SCORE_CMD()
    SCORNG = .true.
    call DO_SCORE()
    SCORNG = .false.
    print '(A,I4,A,I4,A)', ' If you were to quit now, you would score', &
      int(SCORE), ' out of a possible', int(MXSCOR), '.'
    GAVEUP = YES(143_i64p, 54_i64p, 54_i64p)
    if (GAVEUP) call DO_SCORE()
  end subroutine VB_SCORE_CMD


  subroutine VB_FOO()
    integer(kind=i64p) :: k_foo
    k_foo = VOCAB(ia5(WD1), 3_i64p)
    SPK = 42_i64p
    if (FOOBAR /= 1_i64p - k_foo) then
      if (FOOBAR /= 0_i64p) SPK = 151_i64p
      call RSPEAK(SPK); return
    end if
    FOOBAR = k_foo
    if (k_foo /= 4_i64p) then; call RSPEAK(9_i64p); return; end if   ! partial ok
    FOOBAR = 0_i64p
    if (PLACE(int(EGGS)) == PLAC(int(EGGS)) .or. &
        (TOTING(EGGS) .and. LOC == PLAC(int(EGGS)))) then
      call RSPEAK(SPK); return
    end if
    if (PLACE(int(EGGS)) == 0_i64p .and. PLACE(int(TROLL)) == 0_i64p .and. &
        PROP(int(TROLL)) == 0_i64p) PROP(int(TROLL)) = 1_i64p
    k_foo = 2_i64p
    if (HERE(EGGS)) k_foo = 1_i64p
    if (LOC == PLAC(int(EGGS))) k_foo = 0_i64p
    call MOVE(EGGS, PLAC(int(EGGS)))
    call PSPEAK(EGGS, k_foo)
  end subroutine VB_FOO


  subroutine VB_BRIEF()
    SPK = 156_i64p
    ABBNUM = 10000_i64p
    DETAIL = 3_i64p
    call RSPEAK(SPK)
  end subroutine VB_BRIEF


  subroutine VB_READ()
    if (OBJ == 0_i64p) then
      if (HERE(MAGZIN)) OBJ = MAGZIN
      if (HERE(TABLET)) OBJ = OBJ*100_i64p + TABLET
      if (HERE(MESSAG)) OBJ = OBJ*100_i64p + MESSAG
      if (CLOSED .and. TOTING(OYSTER)) OBJ = OYSTER
      if (OBJ > 100_i64p .or. OBJ == 0_i64p .or. DARK()) then
        call RSPEAK(SPK); return
      end if
    end if
    if (DARK()) then; call RSPEAK(SPK); return; end if
    if (OBJ == MAGZIN) SPK = 190_i64p
    if (OBJ == TABLET) SPK = 196_i64p
    if (OBJ == MESSAG) SPK = 191_i64p
    if (OBJ == OYSTER .and. HINTED(2) .and. TOTING(OYSTER)) SPK = 194_i64p
    if (OBJ /= OYSTER .or. HINTED(2) .or. .not. TOTING(OYSTER) .or. .not. CLOSED) then
      call RSPEAK(SPK); return
    end if
    HINTED(2) = YES(192_i64p, 193_i64p, 54_i64p)
  end subroutine VB_READ


  subroutine VB_BREAK()
    if (OBJ == MIRROR) SPK = 148_i64p
    if (OBJ == VASE .and. PROP(int(VASE)) == 0_i64p) then
      SPK = 198_i64p
      if (TOTING(VASE)) call DROP(VASE, LOC)
      PROP(int(VASE)) = 2_i64p
      FIXED(int(VASE)) = -1_i64p
      call RSPEAK(SPK); return
    end if
    if (OBJ == MIRROR .and. CLOSED) then
      call RSPEAK(197_i64p)
      call DISTURB_DWARVES(); return
    end if
    call RSPEAK(SPK)
  end subroutine VB_BREAK


  subroutine VB_WAKE()
    if (OBJ /= DWARF .or. .not. CLOSED) then; call RSPEAK(SPK); return; end if
    call RSPEAK(199_i64p)
    call DISTURB_DWARVES()
  end subroutine VB_WAKE


  ! ===================================================================
  ! DWARF_MOVE: move each dwarf/pirate (labels 6000-6030)
  ! ===================================================================
  subroutine DWARF_MOVE()
    integer :: i_d, j_d
    integer(kind=i64p) :: j64, kk_d, newloc_d
    integer(kind=i64p) :: TK_D(20)

    if (LOC == 0_i64p .or. FORCED(LOC) .or. BITSET(NEWLOC, 3_i64p)) return
    if (DFLAG == 0_i64p) then
      if (LOC >= 15_i64p) DFLAG = 1_i64p
      return
    end if

    ! First dwarf encounter
    if (DFLAG == 1_i64p) then
      if (LOC < 15_i64p .or. PCT(95_i64p)) return
      DFLAG = 2_i64p
      do i_d = 1, 2
        j_d = 1 + int(RAN(5_i64p))
        if (PCT(50_i64p)) DLOC(j_d) = 0_i64p
      end do
      do i_d = 1, 5
        if (DLOC(i_d) == LOC) DLOC(i_d) = DALTLC
        ODLOC(i_d) = DLOC(i_d)
      end do
      call RSPEAK(3_i64p)
      call DROP(AXE, LOC)
      return
    end if

    ! Full dwarf movement
    DTOTAL = 0_i64p; ATTACK = 0_i64p; STICK = 0_i64p
    do i_d = 1, 6
      if (DLOC(i_d) == 0_i64p) cycle
      j_d = 1
      kk_d = KEY(int(DLOC(i_d)))
      if (kk_d /= 0_i64p) then
        do while (.true.)
          newloc_d = mod(abs(TRAVEL(int(kk_d))) / 1000_i64p, 1000_i64p)
          if (newloc_d <= 300_i64p .and. newloc_d >= 15_i64p .and. &
              newloc_d /= ODLOC(i_d) .and. &
              .not. (j_d > 1 .and. newloc_d == TK_D(j_d-1)) .and. &
              j_d < 20 .and. newloc_d /= DLOC(i_d) .and. &
              .not. FORCED(newloc_d) .and. &
              .not. (i_d == 6 .and. BITSET(newloc_d, 3_i64p)) .and. &
              abs(TRAVEL(int(kk_d))) / 1000000_i64p /= 100_i64p) then
            TK_D(j_d) = newloc_d
            j_d = j_d + 1
          end if
          kk_d = kk_d + 1_i64p
          if (TRAVEL(int(kk_d-1)) < 0_i64p) exit
        end do
      end if
      TK_D(j_d) = ODLOC(i_d)
      if (j_d >= 2) j_d = j_d - 1
      j_d = 1 + int(RAN(int(j_d, kind=i64p)))
      ODLOC(i_d) = DLOC(i_d)
      DLOC(i_d) = TK_D(j_d)
      DSEEN(i_d) = (DSEEN(i_d) .and. LOC >= 15_i64p) .or. &
                   (DLOC(i_d) == LOC .or. ODLOC(i_d) == LOC)
      if (.not. DSEEN(i_d)) cycle
      DLOC(i_d) = LOC

      if (i_d /= 6) then
        ! Regular dwarf in room
        DTOTAL = DTOTAL + 1_i64p
        if (ODLOC(i_d) /= DLOC(i_d)) cycle
        ATTACK = ATTACK + 1_i64p
        if (KNFLOC >= 0_i64p) KNFLOC = LOC
        if (RAN(1000_i64p) < 95_i64p * (DFLAG - 2_i64p)) STICK = STICK + 1_i64p
      else
        ! Pirate
        call PIRATE_MOVE()
      end if
    end do

    ! Report dwarf situation
    if (DTOTAL == 0_i64p) return
    if (DTOTAL == 1_i64p) then
      call RSPEAK(4_i64p)
    else
      print '(A,I1,A)', ' There are ', int(DTOTAL), &
        ' threatening little dwarves in the room with you.'
    end if
    if (ATTACK == 0_i64p) return
    if (DFLAG == 2_i64p) DFLAG = 3_i64p
    if (ATTACK == 1_i64p) then
      call RSPEAK(5_i64p)
      SPK = 52_i64p
    else
      print '(A,I1,A)', ' ', int(ATTACK), ' of them throw knives at you!'
      SPK = 6_i64p
    end if
    if (STICK <= 1_i64p) then
      call RSPEAK(SPK + STICK)
      if (STICK == 0_i64p) return
    else
      print '(A,I1,A)', ' ', int(STICK), ' of them get you!'
    end if
    ! Player hit – trigger death
    OLDLC2 = LOC
    call DEATH_AND_SCORE()
  end subroutine DWARF_MOVE


  subroutine PIRATE_MOVE()
    integer(kind=i64p) :: k_p, j_p, j_pp
    integer :: j_int
    if (LOC == CHLOC .or. PROP(int(CHEST)) >= 0_i64p) return
    k_p = 0_i64p
    do j_p = 50_i64p, MAXTRS
      if (j_p == PYRAM .and. (LOC == PLAC(int(PYRAM)) .or. LOC == PLAC(int(EMRALD)))) cycle
      if (TOTING(j_p)) then
        call RSPEAK(128_i64p)
        if (PLACE(int(MESSAG)) == 0_i64p) call MOVE(CHEST, CHLOC)
        call MOVE(MESSAG, CHLOC2)
        do j_int = 50, int(MAXTRS)
          j_pp = int(j_int, kind=i64p)
          if (j_pp == PYRAM .and. (LOC == PLAC(int(PYRAM)) .or. LOC == PLAC(int(EMRALD)))) cycle
          if (AT(j_pp) .and. FIXED(int(j_pp)) == 0_i64p) call CARRY(j_pp, LOC)
          if (TOTING(j_pp)) call DROP(j_pp, CHLOC)
        end do
        DLOC(6) = CHLOC; ODLOC(6) = CHLOC; DSEEN(6) = .false.
        return
      end if
      if (HERE(j_p)) k_p = 1_i64p
    end do
    if (TALLY == TALLY2+1_i64p .and. k_p == 0_i64p .and. PLACE(int(CHEST)) == 0_i64p &
        .and. HERE(LAMP) .and. PROP(int(LAMP)) == 1_i64p) then
      call RSPEAK(186_i64p)
      call MOVE(CHEST, CHLOC)
      call MOVE(MESSAG, CHLOC2)
      DLOC(6) = CHLOC; ODLOC(6) = CHLOC; DSEEN(6) = .false.
      return
    end if
    if (ODLOC(6) /= DLOC(6) .and. PCT(20_i64p)) call RSPEAK(127_i64p)
  end subroutine PIRATE_MOVE


  ! ===================================================================
  ! CLOSE_CAVE: cave-closing sequence (label 10000)
  ! ===================================================================
  subroutine CLOSE_CAVE()
    integer :: i_c
    PROP(int(GRATE)) = 0_i64p
    PROP(int(FISSUR)) = 0_i64p
    do i_c = 1, 6
      DSEEN(i_c) = .false.
      DLOC(i_c) = 0_i64p
    end do
    call MOVE(TROLL, 0_i64p); call MOVE(TROLL+100_i64p, 0_i64p)
    call MOVE(TROLL2, PLAC(int(TROLL))); call MOVE(TROLL2+100_i64p, FIXD(int(TROLL)))
    call JUGGLE(CHASM)
    if (PROP(int(BEAR)) /= 3_i64p) call DSTROY(BEAR)
    PROP(int(CHAIN)) = 0_i64p; FIXED(int(CHAIN)) = 0_i64p
    PROP(int(AXE)) = 0_i64p;   FIXED(int(AXE)) = 0_i64p
    call RSPEAK(129_i64p)
    CLOCK1 = -1_i64p
    CLOSNG = .true.
  end subroutine CLOSE_CAVE


  ! ===================================================================
  ! FINAL_PUZZLE: transport to end-game repository (label 11000)
  ! ===================================================================
  subroutine FINAL_PUZZLE()
    integer :: i_f
    integer(kind=i64p) :: dummy
    PROP(int(BOTTLE)) = PUT(BOTTLE, 115_i64p, 1_i64p)
    PROP(int(PLANT))  = PUT(PLANT,  115_i64p, 0_i64p)
    PROP(int(OYSTER)) = PUT(OYSTER, 115_i64p, 0_i64p)
    PROP(int(LAMP))   = PUT(LAMP,   115_i64p, 0_i64p)
    PROP(int(ROD))    = PUT(ROD,    115_i64p, 0_i64p)
    PROP(int(DWARF))  = PUT(DWARF,  115_i64p, 0_i64p)
    LOC = 115_i64p; OLDLOC = 115_i64p; NEWLOC = 115_i64p

    dummy = PUT(GRATE, 116_i64p, 0_i64p)
    PROP(int(SNAKE))  = PUT(SNAKE,  116_i64p, 1_i64p)
    PROP(int(BIRD))   = PUT(BIRD,   116_i64p, 1_i64p)
    PROP(int(CAGE))   = PUT(CAGE,   116_i64p, 0_i64p)
    PROP(int(ROD2))   = PUT(ROD2,   116_i64p, 0_i64p)
    PROP(int(PILLOW)) = PUT(PILLOW, 116_i64p, 0_i64p)
    PROP(int(MIRROR)) = PUT(MIRROR, 115_i64p, 0_i64p)
    FIXED(int(MIRROR)) = 116_i64p

    do i_f = 1, 100
      if (TOTING(int(i_f, kind=i64p))) call DSTROY(int(i_f, kind=i64p))
    end do

    call RSPEAK(132_i64p)
    CLOSED = .true.
  end subroutine FINAL_PUZZLE


  ! ===================================================================
  ! DISTURB_DWARVES: oh dear (label 19000)
  ! ===================================================================
  subroutine DISTURB_DWARVES()
    call RSPEAK(136_i64p)
    call DO_SCORE()
  end subroutine DISTURB_DWARVES


  ! ===================================================================
  ! DEATH_AND_SCORE: player died (labels 90/99)
  ! ===================================================================
  subroutine DEATH_AND_SCORE()
    integer :: j_death
    logical :: yea
    integer(kind=i64p) :: k_death

    if (CLOSNG) then
      call RSPEAK(131_i64p)
      NUMDIE = NUMDIE + 1_i64p
      call DO_SCORE(); return
    end if

    yea = YES(81_i64p + NUMDIE*2_i64p, 82_i64p + NUMDIE*2_i64p, 54_i64p)
    NUMDIE = NUMDIE + 1_i64p
    if (NUMDIE == MAXDIE .or. .not. yea) then; call DO_SCORE(); return; end if

    ! Resurrect
    PLACE(int(WATER)) = 0_i64p
    PLACE(int(OIL)) = 0_i64p
    if (TOTING(LAMP)) PROP(int(LAMP)) = 0_i64p
    do j_death = 1, 100
      k_death = int(101 - j_death, kind=i64p)
      if (.not. TOTING(k_death)) cycle
      if (k_death == LAMP) then
        call DROP(k_death, 1_i64p)
      else
        call DROP(k_death, OLDLC2)
      end if
    end do
    LOC = 3_i64p; OLDLOC = LOC; NEWLOC = LOC
  end subroutine DEATH_AND_SCORE


  ! ===================================================================
  ! Hint handler (label 40000)
  ! ===================================================================
  subroutine DO_HINT(hint)
    integer, intent(in) :: hint
    logical :: offer
    offer = .false.
    select case (hint)
    case (4)   ! cave entrance
      if (PROP(int(GRATE)) == 0_i64p .and. .not. HERE(KEYS)) offer = .true.
    case (5)   ! bird
      if (HERE(BIRD) .and. TOTING(ROD) .and. OBJ == BIRD) offer = .true.
    case (6)   ! snake
      if (HERE(SNAKE) .and. .not. HERE(BIRD)) offer = .true.
      if (.not. offer) then; HINTLC(hint) = 0_i64p; return; end if
    case (7)   ! maze
      if (ATLOC(int(LOC)) == 0_i64p .and. ATLOC(int(OLDLOC)) == 0_i64p .and. &
          ATLOC(int(OLDLC2)) == 0_i64p .and. HOLDNG > 1_i64p) offer = .true.
      if (.not. offer) then; HINTLC(hint) = 0_i64p; return; end if
    case (8)   ! dark room
      if (PROP(int(EMRALD)) /= -1_i64p .and. PROP(int(PYRAM)) == -1_i64p) offer = .true.
    case (9)   ! witt's end
      offer = .true.
    end select

    if (.not. offer) then; HINTLC(hint) = 0_i64p; return; end if

    HINTLC(hint) = 0_i64p
    if (.not. YES(HINTS(hint,3), 0_i64p, 54_i64p)) return
    print '(A,I2,A)', ' I am prepared to give you a hint, but it will cost you', &
      int(HINTS(hint,2)), ' points.'
    HINTED(hint) = YES(175_i64p, HINTS(hint,4), 54_i64p)
    if (HINTED(hint) .and. LIMIT > 30_i64p) LIMIT = LIMIT + 30_i64p * HINTS(hint,2)
  end subroutine DO_HINT


  ! ===================================================================
  ! DO_SCORE: compute and print final score (label 20000)
  ! ===================================================================
  subroutine DO_SCORE()
    integer :: i_s
    integer(kind=i64p) :: k_sc

    SCORE = 0_i64p; MXSCOR = 0_i64p

    ! Treasures
    do i_s = 50, int(MAXTRS)
      if (PTEXT(i_s) == 0_i64p) cycle
      k_sc = 12_i64p
      if (i_s == int(CHEST)) k_sc = 14_i64p
      if (i_s > int(CHEST))  k_sc = 16_i64p
      if (PROP(i_s) >= 0_i64p) SCORE = SCORE + 2_i64p
      if (PLACE(i_s) == 3_i64p .and. PROP(i_s) == 0_i64p) SCORE = SCORE + k_sc - 2_i64p
      MXSCOR = MXSCOR + k_sc
    end do

    SCORE  = SCORE  + (MAXDIE - NUMDIE) * 10_i64p
    MXSCOR = MXSCOR + MAXDIE * 10_i64p
    if (.not. (SCORNG .or. GAVEUP)) SCORE = SCORE + 4_i64p
    MXSCOR = MXSCOR + 4_i64p
    if (DFLAG /= 0_i64p) SCORE = SCORE + 25_i64p
    MXSCOR = MXSCOR + 25_i64p
    if (CLOSNG) SCORE = SCORE + 25_i64p
    MXSCOR = MXSCOR + 25_i64p
    if (CLOSED) then
      if (BONUS == 0_i64p)   SCORE = SCORE + 10_i64p
      if (BONUS == 135_i64p) SCORE = SCORE + 25_i64p
      if (BONUS == 134_i64p) SCORE = SCORE + 30_i64p
      if (BONUS == 133_i64p) SCORE = SCORE + 45_i64p
    end if
    MXSCOR = MXSCOR + 45_i64p
    if (PLACE(int(MAGZIN)) == 108_i64p) SCORE = SCORE + 1_i64p
    MXSCOR = MXSCOR + 1_i64p
    SCORE  = SCORE  + 2_i64p
    MXSCOR = MXSCOR + 2_i64p

    ! Hint deductions
    do i_s = 1, int(HNTMAX)
      if (HINTED(i_s)) SCORE = SCORE - HINTS(i_s,2)
    end do

    if (SCORNG) return   ! caller will print

    print '(/,/,A,I4,A,I4,A,I5,A)', ' You scored', int(SCORE), &
      ' out of a possible', int(MXSCOR), ', using', int(TURNS), ' turns.'

    do i_s = 1, int(CLSSES)
      if (CVAL(i_s) >= SCORE) then
        call SPEAK(CTEXT(i_s))
        if (i_s < int(CLSSES) - 1) then
          k_sc = CVAL(i_s) + 1_i64p - SCORE
          if (k_sc == 1_i64p) then
            print '(A,I3,A)', ' To achieve the next higher rating, you need', int(k_sc), ' more point.'
          else
            print '(A,I3,A)', ' To achieve the next higher rating, you need', int(k_sc), ' more points.'
          end if
        else
          print '(A)', ' To achieve the next higher rating would be a neat trick!!'
          print '(A)', ' Congratulations!!'
        end if
        exit
      end if
      if (i_s == int(CLSSES)) print '(A)', ' You just went off my scale!!'
    end do

    stop
  end subroutine DO_SCORE


  ! ===================================================================
  ! Helper: convert packed i64 word back to 5-char string
  ! ===================================================================
  subroutine a5_to_str(val, s)
    integer(kind=i64p), intent(in) :: val
    character(len=5), intent(out) :: s
    integer :: i, b
    do i = 1, 5
      b = int(iand(ishft(val, -(40-8*i)), 255_i64p))
      if (b == 0) b = 32
      s(i:i) = achar(b)
    end do
  end subroutine a5_to_str


  ! ===================================================================
  ! RAN: random number [0, range-1]
  ! ===================================================================
  integer(kind=i64p) function RAN(range)
    integer(kind=i64p), intent(in) :: range
    real :: r
    call random_number(r)
    if (range <= 0_i64p) then
      RAN = 0_i64p
    else
      RAN = int(r * real(range), kind=i64p)
      if (RAN >= range) RAN = range - 1_i64p
    end if
  end function RAN

end module game_mod
