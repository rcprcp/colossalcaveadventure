! Main program entry point for the Fortran 2023 ADVENT conversion.
program advent_main
  use advent_mod
  use compat_mod
  use section_parsers_mod
  implicit none

  print *, 'Starting Fortran2023 converted ADVENT (stage 2 WIP)'
  print *, 'Reading ADVENT data file: ', trim(DATA_FILENAME)

  call read_database()

  call finalize_database()

  print *, 'Database read (WIP). Continuing conversion in further commits.'

  ! The original program immediately did initialization based on SETUP.
  ! Call POOF as a placeholder until full game loop is wired in.
  call POOF()
  print *, 'WIP scaffolding initialization complete.'

end program advent_main
