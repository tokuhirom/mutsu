# ADR-0109 proposed: a native `is rw` scalar parameter reaches the light-call fast paths (#8686 Phase 2)

#8686's Phase 2 — admitting a simple native `is rw` scalar parameter to the light-call fast paths, the
actual mechanism `JSON::Fast`'s own headline workload (#8673) is blocked on — was explicitly gated by
the issue on writing a `Proposed` ADR before any implementation attempt, per this repo's own
architecture-decision policy. [ADR-0109](../../docs/adr/0109-native-is-rw-scalar-parameter-reaches-the-light-call-fast-paths.md)
is that ADR.

## What it decides

Rather than building new aliasing machinery for the light paths, the ADR found — by reading the
compiler's call-argument compile path and the general binder's existing `is rw` handling, not by
assuming — that every plain-lexical or plain-assignment positional argument (`nom-ws($text, $pos)`,
`parse-string($text, $pos = $pos + 1)`, the two shapes `JSON::Fast`'s own helpers use) is *already*
tagged at the call site with a cheap `WrapVarRef`/`VarRef` wrapper, for every call, regardless of which
binder ends up processing it. Promoting that tag into a real shared `ContainerRef` cell is one existing,
reusable primitive (`Interpreter::capture_var_cell`), and a local slot already reads/writes through a
`ContainerRef` transparently once it holds one — so no new opcode or storage mechanism is needed for
the parameter's own slot.

The ADR scopes Phase 2 narrowly to that exact shape: a parameter whose only trait is `rw`, bound from a
plain lexical or assignment-expression argument. It adds a runtime, per-call admission check (mirroring
the existing `positional_light_full_arity_call` precedent) that declines to the general binder — safely,
with no behavior change — for every other argument shape (an accessor read, a subscript element, a
literal, a sigilless bareword). `is raw`, sigilless (`\x`) parameters, and non-lexical argument sources
are deliberately left out of this slice; the general binder's fuller `is rw`/`is raw` machinery
(`pending_rw_writeback_slots`, alias-chain resolution, the `is raw` implicit-veto rules) is reused only
by declining to it, never replicated inside the light path.

## Still open on #8686

The ADR is `Proposed`, not yet implemented. Phase 1's first bullet is
[#8690](https://github.com/tokuhirom/mutsu/issues/8690). Phase 3 (re-profile once Phases 0-2 land) is
unstarted. #8686 stays open.
