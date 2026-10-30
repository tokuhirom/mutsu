# END does not see the mainline's `my $*x`

Rakudo runs `END` with no caller chain, so a dynamic variable declared with `my $*x` in the
mainline (or in a block holding the `END`) is not visible there; process-level dynamics
(`$*OUT`, `$*CWD`, ...) still are. mutsu kept the mainline binding alive and captured the
declaring block's dynamics into the phaser. Red `t/91-cross-product` tears its tables down in an
`END { ... if $*RED-DB.defined }` guard, which raku skips and mutsu ran (`^drop` does not exist).
