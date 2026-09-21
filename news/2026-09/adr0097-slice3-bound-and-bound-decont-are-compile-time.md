# ADR-0097 slice 3 splits the same way slice 2 did — and `bound`/`bound_decont` turn out to need no runtime state at all

Investigating [ADR-0097](../../docs/adr/0097-a-binding-descriptor-addressed-by-slot.md)
§5 slice 3 (`sigilless_alias`, `sigilless_readonly`, `scalar_bind_no_container`,
`bound`, `bound_decont`, `var_source_name`, `outer`) before writing any code,
continuing [#8069](https://github.com/tokuhirom/mutsu/issues/8069) after
§10 and §12's earlier rounds on slice 2.

## The grouping is not uniform, again

Five of the seven properties are dynamic per-execution facts, the same shape
§10 found for `shaped_array_dims`: the value each one holds (an alias-chain
target name, a per-call parameter-binding source, a per-closure-creation
snapshot) is decided at runtime and can differ across repeated executions of
the same declaration. All five need "the runtime half" ADR-0097 §2.2
sketched — a frame-lifetime, slot-indexed store that does not exist yet — and,
this round found, that half cannot be built as a plain array parallel to
`Locals` (ADR-0077) without first answering how it survives a closure
capture: today every one of these five is *also* readable through a
closure's flattened captured `env`, and a `Locals`-only side table would
silently lose the fact the moment a closure reaches it. This generalizes
ADR-0097 §11.5's "closure capture is not covered at all", filed against one
specific latch (`LOCAL_READ_SPOILERS`) — it is actually the blocker for the
runtime half as a whole, and is the thing to design next, not any one
property's plumbing.

## `bound` and `bound_decont` are the exception

Their write sites are each one fixed compiled statement (`Stmt::MarkBoundContainer`,
the coerce-path bind marker), and `Compiler::alloc_fresh_local` never reuses a
local slot across declarations — every `my` gets a permanently fresh slot
number. So "is slot N a `:=`-bound container" is decided by which single AST
statement compiled to that slot, at compile time, for any name that resolves
to a local — no runtime write or read is needed for that case at all, unlike
the five above. The "clear a stale marker" logic that exists elsewhere in
this codebase for sibling namespaces is a symptom of *name*-keying (two
different slots sharing a name can collide on the same env key); it has no
equivalent failure mode once the fact is addressed by a declaration's own
permanent slot.

This makes `bound`/`bound_decont` the next concrete, scoped ADR-0097 slice —
and a smaller one than it first looked: an initial call-site count using a
plain substring `grep` on `MetaNs::Bound` silently folded in every sibling
namespace whose name starts with `Bound` (`BoundIndex`, `BoundArrayLen`,
`BoundArraySlice`, `BoundDecont`), inflating the apparent count to ~20; a
word-boundary count puts the real number at 5 call sites each. What is not
yet verified, and should be checked before implementing: `MarkBoundContainer`
compiles to `SetGlobal`, so it may also target names with no local slot
(`our %a := ...`), which would need the same partial-retirement treatment
already accepted for `type`/`hash_key_type` in §10.

No code changes this round. Full writeup is ADR-0097 §13.
