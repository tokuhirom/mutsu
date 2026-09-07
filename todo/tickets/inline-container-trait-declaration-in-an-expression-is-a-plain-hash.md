# `(my %q is SetHash)` used as an expression is a plain `Hash`

Found 2026-09-07 while working
`todo/tickets/set-smartmatches-its-mutable-counterpart.md`
(`news/2026-09/quanthash-mutability-is-not-a-subtype.md`). It is independent of
that fix — it reproduces identically before and after it, measured on both
builds.

## Repro

```raku
say (my %q is SetHash).^name;      # raku: SetHash   mutsu: Hash
say (my %q is SetHash) ~~ SetHash; # raku: True      mutsu: False
```

The same holds for `is BagHash` and `is MixHash`.

## Narrowed — it is the expression position, not the trait

Declared as its own statement and read afterwards, the trait is applied and
everything agrees:

| Program | raku | mutsu |
|---|---|---|
| `my %q is SetHash; %q.^name` | `SetHash` | `SetHash` — correct |
| `my %q is SetHash; %q ~~ SetHash` | `True` | `True` — correct |
| `my %q is SetHash; %q ~~ Set` | `False` | `False` — correct |
| **`(my %q is SetHash).^name`** | `SetHash` | **`Hash`** |

So the container trait *is* applied — just not before the declaration's own
value is handed to the surrounding expression. mutsu applies `is <Type>`
through a separate `ApplyVarTrait` op that runs after the declaration pushes
its value, so an expression that consumes the declaration directly sees the
un-coerced `Hash`.

## Where to look

The `ApplyVarTrait` emission order in `src/compiler/stmt.rs` relative to the
declaration's own value push, and `src/vm/vm_var_trait_ops.rs` (which reads the
declared slot back to coerce it). The declaration's *result value* has to be
the coerced container, not the raw `Hash` that was in the slot when the
declaration op ran.

## Neighbourhood to check when fixing

`(my @a is Buf)`; `(my $x is Foo)`; a declaration-with-trait as the last
statement of a block (whose value becomes the block's result); the same inside
a `given`/`with` topic; and the `ApplyVarTrait` interaction recorded in
`todo/tickets/typed-container-capture-still-loses-to-a-same-named-caller-array.md`,
which is the other open ticket on this op.
