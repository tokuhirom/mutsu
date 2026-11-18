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

A module mainline is now compiled as a unit mainline whatever loads it: its
top-level subs bind no per-activation aliases and do not take its file-scope
`my`s for an enclosing routine's variables, so they read the module's own
compilation-unit store after the load. (It still runs in the loader's env, so a
trait the module runs while loading resolves the module's constants as
before; DBIish's lazily required mysql driver needs that.) The file-scope
lexical lookup also answers the loading module while no routine has been
called since its load began. Monad's `t/12_conversions.t` matches `raku` again.
