# Prefix `~` no longer routes `.Stringy` into a class's `FALLBACK`

`~$obj` on an instance whose class defines `method FALLBACK($name, $val, |c)` but no
`Stringy` dispatched `.Stringy` into the `FALLBACK`, which died with "Too few positionals".
Rakudo's `~` calls `.Str`, so the `Stringy` probe is now skipped in that case. Found via
CSS::Writer, whose `t/00basic.t` and `t/01readme.t` now pass; `t/node-doco.t` still needs #11176.
