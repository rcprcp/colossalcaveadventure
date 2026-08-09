WIP notes for reviewers

- I added a compatibility module (advent_f2023.f90) with 64-bit kinds and portable bit/shift helpers.
- I created an initial conversion file (advent_f2023_full.f90) which now reads advent.dat using read_database(), dispatches sections via SELECT CASE, and contains handler stubs for each section.
- This push adds an initial implementation file for message section readers (wip/section_readers.f90) with an outline of how I'll parse the fixed-width records into the LINES array. It's incomplete and meant to show intent for reviewers.

Next steps

- Complete parsing logic for sections 1,2,5,6,10,12: store text lines in LINES and set LTEXT/STEXT/PTEXT/RTEXT/CTEXT/MTEXT pointers exactly like original.
- Implement Section 3 travel table reader (TRAVEL, KEY) and Section 4 vocabulary parsing (KTAB/ATAB) with the correct obfuscation/hashing reverse.
- Replace remaining placeholders and stubs; iteratively compile and test initialization.
