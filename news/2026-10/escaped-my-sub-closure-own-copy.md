# An escaped anonymous method calls the `my sub` of its own method

A `my sub` declared in a method body is a lexical of that method, but a call to
it from an `anon method`/`anon sub` literal inside the method resolved by name
through the routine registry, which holds only the last-registered same-named
sub: two methods each declaring `gs`, each returning an `anon method` that calls
it, both closures called the second method's `gs` (`b1b1` where Rakudo prints
`a1b1`). The closure-body compiler withheld the enclosing block's routines from
routine-shaped literals (block-like closures already read them as bindings);
they now inherit them too. Fixes #13550.
