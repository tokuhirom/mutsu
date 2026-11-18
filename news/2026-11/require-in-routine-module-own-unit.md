# A module required inside a routine runs as its own compilation unit

Monad's `t/12_conversions.t` had dropped to 2/8: `Monad::Maybe.to-either`
reaches `Monad::Either` through a lazy `require ::('Monad::Either')` inside a
routine, and the required module's subs then read its file-scope
`my $type-cache` as `Any` (#18017).

Two things went wrong because the load ran under the requiring routine's
frame. The module's mainline was compiled with the routine's context, so its
top-level subs became routine-nested subs whose free variables bind per
activation of a declaring routine that does not exist. And while the mainline
ran, a by-name write of its own `my $cache` resolved to the running routine's
compilation unit, overwriting the loader's same-named file-scope lexical.

A module mainline is now always compiled at the top of its own unit, whatever
loads it, and the file-scope lexical lookup answers the loading module while
no routine has been called since its load began. Monad's whole suite matches
`raku` again.
