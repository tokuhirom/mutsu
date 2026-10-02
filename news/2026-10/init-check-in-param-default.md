# INIT/CHECK in a parameter default runs at init time

An `INIT` or `CHECK` phaser written in a parameter default (`sub ($a = INIT { ... })`) now runs
once before the mainline, as in rakudo, instead of at the first call that uses the default. The
phaser lift (`src/runtime/phasers/lift.rs`) no longer skips parameter defaults; the default reads
the lifted value, and the case is pinned in `t/control/init-check-lift-positions.t` (#10551).
