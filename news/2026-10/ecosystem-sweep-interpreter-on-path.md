# Ecosystem sweep puts the measured interpreter on PATH

`$*EXECUTABLE-NAME` is the interpreter's basename on both rakudo and mutsu, so a test suite that
does `run($*EXECUTABLE-NAME, ...)` (App::Unicode::Mangle) only works when that interpreter is on
`PATH`. `raku` always was; `MUTSU_BIN=target/release/mutsu` was not, so nine of the distribution's ten
files failed on mutsu alone, which is a harness asymmetry rather than an interpreter gap.
`scripts/ecosystem-sweep.py` now prepends the directory of whichever interpreter it runs to `PATH`.
App::Unicode::Mangle re-measures `green`, 10/10 files at parity.
