# Start-up code is laid out together, cutting `bench-startup`'s page faults

Before this change, `say "hello"` executed about 1,700 functions spread over mutsu's ~50 MB
text segment. The kernel maps file-backed code in 64 KiB fault-around windows, so the run
touched ~400 windows and kept ~27 MB of code resident. Raku++'s whole binary is 6.4 MB
resident. The extra kernel time was most of mutsu's gap to Raku++ on `bench-startup` (#19197).

`build.rs` now passes `linker/startup-text.ld` to the link of the `mutsu` binary. It is an lld
script that gathers the functions start-up executes into one output section in front of
`.text`. `scripts/gen-startup-layout.py` writes the script from callgrind traces of the empty
program and of `bench-startup`. The script only changes where code is placed:

- Each function is named by a section pattern in which the crate hashes are wildcards, so the
  file keeps working across version bumps and toolchain updates.
- A pattern that no longer matches is ignored, so a stale file falls back to the old layout
  instead of breaking the build.
- `--check` reports how much of the file still matches the current binary.

The script applies to optimized `x86_64-unknown-linux-gnu` builds that use the default linker,
which covers the release tarball and the bench job. It does not apply to builds with mold or
another custom linker, or when `MUTSU_STARTUP_LAYOUT=0` is set.

The numbers below are from a CI-style release build (non-incremental), `bench-startup` with
`MUTSU_JIT=off` on a 4-core container:

- Minor faults went from ~1,396 to ~1,217.
- Resident text went from 27.4 MB to 8.8 MB.
- Median wall time fell 5-7% over 2x40 interleaved runs.

Linking the release binary takes ~3 s longer.

Most of the faults that remain are not code. About 400 are copy-on-write faults from the
dynamic loader's relocations of `.data.rel.ro`. About 250 come from first-touched pages of the
interpreter thread's heap. The issue stays open for those.
