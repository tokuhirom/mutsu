# `DYNAMIC::<$y>` skips a dormant nested block's `my $y`

`DYNAMIC::<$y>` for a plain lexical used to answer `Any` when a routine on the
call stack declared `my $y` in a nested block that was not running, because
the routine's slot table names the inlined block's slot too. A slot declared
only inside a nested `BlockScope` that reads unset is now skipped, so the walk
reaches the real owner (#19182).
