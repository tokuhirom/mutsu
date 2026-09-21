# A typed lexical's declared constraint stops being an env probe per store

`my int $i` settles `$i`'s type constraint at its declaration. mutsu re-derived
it from the environment **on every store to `$i`** — hash `__mutsu_type::i`,
walk the env chain, decode the `Str` it found there — to re-learn a fact the
compiler had already read out of the source.

Measured on the `nqp::add_i` loop from
[#8877](https://github.com/tokuhirom/mutsu/issues/8877), 100k iterations,
`--profile profiling`:

| | calls | Ir |
| --- | ---: | ---: |
| `var_type_constraint_value_sym` | 100,000 | 6,600,135 |
| `Env::get_sym` (all but 32 of them from the same store) | 100,032 | 7,201,462 |
| **total** | **one per store** | **13,801,597 — 9.18% of the loop** |

That is **138 instructions per store**, every one of them spent on a question
the declaration answered once.

## The slot knows

This is an [ADR-0097](../../docs/adr/0097-a-binding-descriptor-addressed-by-slot.md)
slice: a binding's own metadata belongs on a descriptor addressed by its slot,
not under a key derived from its name. `BindingDesc` grows a
`declared_constraint` field, and the compiler fills it in at the one place it
emits a `SetVarType*` op.

The field is deliberately coarse, because the store asks exactly one question —
`native_typed_store_is_identity`, "would the typed branch leave this value
alone?" — whose answer depends only on which native family the constraint
belongs to:

* `NativeInt` (`int`, `int64`), `NativeStr` (`str`), `NativeNum` (`num`,
  `num64`) — the store is the identity for a value already carrying that tag;
* `NonNative` — a narrow width wraps, a class constraint type-checks, so the
  typed branch always has real work and the fast path declines. This is the same
  verdict the probe reached, so `my Int $x` gets the saving too;
* `Unrecorded` / `Conflicting` — fall through to the env probe, exactly as
  before.

## Why it is not a memo

The issue ruled out the two obvious caches, and both for the same reason: the
env's answer depends on *which env is current*, and a frame change invalidates
it with no mutation to observe. There are 63 sites in `src/` that assign `.env`.

A compile-time bake has no such dependency — it is not a cache of the env's
answer, it is the declaration read at the place the declaration is written. What
it needs instead is a story for the cases where one slot has more than one
declaration, and those are handled by *poisoning*, not by guessing:

* **Two different constraints on one slot.** In the default build a nested
  `my $x` reuses the **outer** slot (`Compiler::declare_local`), so
  `{ my int $p } { my int8 $p }` really is one slot with two constraints. The
  env, which is scoped, can tell them apart; the bake cannot, so it records
  `Conflicting` and defers.
* **A trait.** `ApplyVarTrait`'s container and class branches rewrite the
  constraint at run time (`vm_var_trait_ops`), so every trait emission poisons
  its slot.
* **A subset.** `subset num of Int` redirects a native constraint name. The
  existing `subsets.is_empty()` guard already covered this and still runs first.
* **`@a` / `%h`.** `parse_container_constraint` rewrites a container's
  constraint (`my Int %h{Str}` splits into a value type and a key type), so the
  bake would not match what the env holds. Only plain scalars — the slots the
  store fast path serves anyway — are baked.

**The `__mutsu_type::` env entry is not retired.** It stays the source of truth
for its ~120 other readers and the fallback for the two poisoned states. One
`MetaNs` namespace per field added is the ADR's plan; this slice takes the hot
*reader* off it, not the namespace.

## Measured

| | before | after | |
| --- | ---: | ---: | ---: |
| `my int $i` + `nqp::add_i` loop, 100k | 150,340,494 | see PR | |
| `my Int $x` store loop, 100k | 500,580,544 | see PR | |

Pinned by `t/vm/binding/typed-scalar-store-declared-constraint.t`, whose 19
assertions are each a shape where answering from the declaration instead of the
env could diverge — `int8` still wrapping, one slot with two constraints, a
typed declaration followed by an untyped one of the same name, a subset
redirecting a native name, a trait on a typed slot. All 19 produce byte-identical
output under rakudo.
