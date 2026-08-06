# Scalar `is rw` parameters bind shared container cells

Raku binds an `is rw` parameter to the caller's *container*, so in a wrap
chain every layer aliases one container: the wrapper's rw param, the
callsame'd original's rw param, and the caller's variable all observe a
single write. mutsu implemented rw as a bind-time value copy plus an exit
writeback (`apply_rw_bindings_to_env`), so the chain relay survived only by
coincidence — when the wrapper's and the original's rw params shared a name,
the interpreter carrier's blanket same-name env merge leaked the value
through. Renaming the wrapper's param broke the relay on every path:

```raku
sub w5($x is rw) { $x = $x + 1; $x }
&w5.wrap(sub ($y is rw) { my $r = callsame(); note "wrapper-y=$y"; "w5:$r" });
my $e = 40;
say w5($e);   # raku: wrapper-y=41 / w5:41 — mutsu printed wrapper-y=40
say $e;       # raku: 41 — mutsu printed 40
```

The binder (`bind_function_args_values`) now binds a plain scalar
`is rw`/`is raw` positional parameter to a shared `ContainerRef` cell: it
reuses the caller's live cell when the source variable already holds one (an
outer rw param's alias, a boxed captured lexical), otherwise it boxes the
type-checked value and installs the cell under the caller's source name in
the callee env. Body writes go through the cell (the same write-through the
box-on-capture and `state`-cell mechanisms already use), so no copy-back is
needed for correctness — the existing exit writeback machinery was already
cell-safe (ptr-equal cells skip, plain targets adopt the cell) and now also
records the rw sources from the interpreter carrier so a compiled caller's
local slot picks the cell up (`apply_pending_rw_writeback`). The carrier's
exit merge additionally propagates a `ContainerRef` installed under a
caller-known name during the call, which is the wrap chain's last hop back
to the caller frame.

This is the sound-mechanism choice CLAUDE.md's gain/risk section prescribes
(a shared cell cannot go stale; snapshot-plus-writeback was last-writer-wins
over stale copies), and it unblocked ADR-0019 C6d-4's rw gate: routines with
scalar rw/raw params now run their compiled bodies through `call_sub_value`
like every other routine (the gate in `resolution_call_sub.rs` is deleted).
A visible semantic win on the way: a closure over an rw param now keeps the
caller alias after the sub returns (`sub f($x is rw) { -> { $x++ } }`), as
Raku specifies.

Element-indexed sources (`f(@a[0])`) keep the exit copy-back — their storage
is an array slot, not an env entry a cell can replace. Named `is rw` params
remain a pre-existing gap, now recorded in
`todo/tickets/named-rw-param-writeback-is-lost.md`.

Pinned by `t/rw-shared-cell.t` (14 cases, verified against raku, JIT on/off).
The proto `{*}` half of the missing-alias family is
`news/2026-08/proto-rw-writeback-chains-through-cells.md`.
