# `given`/`with EXPR -> $v is rw { ... }` now writes back a scalar pointy parameter's mutation

A `given`/`with` pointy-topic block binding a scalar (`given EXPR -> $v is rw
{ ... }`) silently lost any mutation made to `$v` — a plain variable source,
a hash/array element source, and both `given` and `with` were all affected.
Aggregate pointy params (`-> @p`/`-> %p`) already wrote back correctly; only
the scalar case was broken.

```raku
my $x = 1;
given $x -> $v is rw { $v += 10 }
say $x;   # raku: 11 — mutsu (before this fix): 1
```

## Root cause

The pointy param's own binding compiles to a synthetic `MarkBind` + `VarDecl`
statement inserted as the body's first statement (`pointy_topic_bind`,
`src/parser/stmt/control.rs`). Because that statement makes the compiled body
"declare a block-local", the whole body is wrapped in an `OpCode::BlockLocalScope`
(`exec_block_local_scope_op`, `src/vm/vm_control_ops.rs`) — the same mechanism
an ordinary `if`/`unless`/`given`/`with` branch uses to keep a body-local `my`
from leaking past the block. On exit that scope Nil-resets every
block-declared name's local slot, including the pointy param's own — and this
runs as part of body execution, i.e. **before** `exec_given_op`'s own
writeback (`write_back_given_topic`/`write_back_element_source`,
`src/vm/vm_loop_writeback.rs`) gets a chance to read the parameter's final
value. A scalar plain lexical has no other home for that value either: the
`(B)` per-store env-write gate (`docs/lexical-scope-slot-campaign.md`) skips
its `env` mirror, so once the slot is wiped the value is gone from both
stores. (An aggregate's env mirror is written unconditionally on every
assignment, which is why `-> @p`/`-> %p` were unaffected.)

## Fix

`exec_given_op` now determines its own pointy param's compiled slot ahead of
time (by peeking the body for the first `SetLocalDecl` — always that param's
own synthetic declaration, since it is always the body's first statement) and
registers it in a new `given_pointy_protect`/`given_pointy_captured` pair of
stacks. `exec_block_local_scope_op` captures that slot's live value into
`given_pointy_captured` immediately before its normal Nil-reset, and
`exec_given_op`'s writeback reads the captured value instead of a (by then
already-wiped) slot/env lookup.

The match is keyed by exact **slot index**, not by variable name: an earlier
attempt keyed by name broke two distinct cases discovered while fixing this —
a pointy param shadowing an outer variable of the same name
(`given 5 -> $x {...}` inside `my $x = 1`, where a by-name reset would
corrupt the *outer* `$x`), and two nested `given`/`with` pointy scalars
sharing a name (`given $a -> $v { given $b -> $v { ... } }`, where
`exec_block_local_scope_op`'s `owned_slots` set spans the whole — possibly
nested — body, so a by-name match could pick up a same-named nested scope's
already-reset slot instead of the right one). Slot indices are unique per
declaration under the shadow-slots architecture, so matching on them is
unambiguous in both cases.

pin = `t/given-with-scalar-pointy-rw-writeback.t` (plain scalar, hash
element, array element, `with`, same-name outer shadow, same-name nesting).

## Related, not fixed here

A scalar pointy param **without** `is rw` should die on assignment
("Cannot assign to a readonly variable") per Raku — mutsu does not enforce
that; tracked separately in
`todo/tickets/given-with-pointy-scalar-missing-readonly-enforcement.md`.
