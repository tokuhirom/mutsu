# A block's imported `trait_mod` candidates bind lexically

A `use` inside a block or routine body binds what it imports in the block's
lexical import set (ADR-8751). Exported `multi trait_mod:<is>` candidates were
the one multi family still aliased into the routine registry for the block's
dynamic extent. A routine whose body says `use Test` (Test exports
`trait_mod:<is>(Routine, :$test-assertion!)`) re-runs that `use` on every call,
and each run put the candidate back into the registry and the routine's exit
took it out again. Every such change flushes all the routine-resolution caches,
so each call also re-resolved its callers' routines: `sub f { use Test; 1 }`
cost about 48k instructions per call, against about 5k without the `use`.

The family now binds in the set as a dispatcher (#8751). Trait application
tries it first and falls back to the by-name dispatch for a trait none of its
candidates accepts, which keeps the built-in traits and the program's own
candidates. This is the path a `sub EXPORT` hook's `&trait_mod:<is>` already
took. The method, class, role, deferred-parent and package trait sites, which
still went straight to the registry, now take that path too. A routine with
such a `use` no longer touches the registry on later calls: a module exporting
one `trait_mod:<is>` candidate went from 292M to 84M instructions for 8000
calls, the cost of a module exporting plain subs.

`t/modules/import-export/use-in-block-binds-trait-mod-lexically.t` pins the
behaviour against rakudo.
