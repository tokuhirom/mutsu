# A module's own imported role resolves for `does` when loaded from an EVAL in a routine

`use-ok 'Mod'` (an `EVAL "use Mod"` inside a top-level routine) hid the short
type aliases that `Mod`'s own `use` of an already-loaded module had just
installed, because the routine's frame hides block-scoped imports (ADR-8751).
A role composed with `does` then failed with `Unknown role: ChainedOperator`.
The hiding check now only applies when no module body is loading. Found with
Qwiratry::Test, whose `t/00-use.rakutest` now matches raku.
