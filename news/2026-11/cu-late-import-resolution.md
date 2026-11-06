# `$*CU` calls of routines imported after the tree was built resolve by CHECK time

A module's `sub EXPORT` that reads `$*CU` (as `static::CLI` does through
`-Mstatic::CLI`) runs before the importing unit's own `use` lines. The tree it
got was resolved against the imports visible then, so a call of a routine
imported later stayed unresolved and the checker reported
`initialization of $n; expected Int, got List` for `first-value() + second-value()`.

The tree is immutable, so the unit now records, just before its queued check
code runs, which unresolved calls name a routine imported by then
(`unit_source_text::publish_late_imports`); `Lookup.is-resolved` / `.resolution`
of such a call answer the `Declaration::Import` from that set. Rakudo resolves
in place before CHECK; the observable result is the same.

Verified on `static` 0.0.1: the #16468 repro prints `Syntax OK` like rakudo.
