# `$s ~~ s:g///` no longer materializes every occurrence's Match

`$copy ~~ s:g/ <[0..9]>+ /#/` returns the list of every occurrence's Match, and
the `~~` op then walks its right-hand value looking for regexes whose
interpolated variables must be published before they run
(`smartmatch_value_sync`). That walk read each element through `view()`, which
forces a lazy Match to build its Instance-shaped attribute map. So every
substitution through `~~` materialized one Match per occurrence, only to drop
them all when `$/` was next assigned.

A Match holds no regex the smartmatch runs, the same as the eager Instance it
presents as, so the walk now skips lazy Matches without forcing them.

On the `s:g///` line of `bench-regex-global.raku` (160 occurrences per
subject, `MUTSU_JIT=off`, callgrind, per operation): **1.69M -> 0.91M
instructions (-46%)**, and the line's timed loop went from 9.6 ms to 5.0 ms
locally. It now costs the same as `.subst(rx, 'x', :g)` on the same pattern.
