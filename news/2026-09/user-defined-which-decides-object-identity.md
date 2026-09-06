# A user-defined `WHICH` now decides object identity everywhere

A class may override `WHICH` to give its instances *value* semantics. mutsu
honoured that override when the method was called directly, but nowhere else:

```raku
my class A {
    has $.a;
    method WHICH { ValueObjAt.new: "A|$!a.WHICH()" }
}
say Set(A.new(a => 5)) eqv Set(A.new(a => 5));   # raku True, mutsu False
```

The ticket that reported this (from the 2026-09-06 doc-diff sweep of
`Language/operators.rakudoc:2376`) blamed the `Set`'s own `.WHICH`, and noted
that object hashes "already use `value_which_key`, so the machinery exists".
Both halves turned out to be wrong, and the real defect was considerably wider.

## Root cause

`runtime::utils::value_which_key` — the single function that computes the
identity string used for Set/Bag/Mix element keys, object-hash keys and
`Set.WHICH` — keyed an `Instance` as `"{Type}|{id}"`, its per-object id. It
never consulted a user `WHICH`, and `runtime::utils::values_identical` (which
backs `===`) compared instances by that same raw id. So the object-hash path
the ticket held up as the working reference was broken in exactly the same way,
and so was everything downstream. Measured against raku v2026.07, the following
were all wrong before this change, not just the reported `eqv` row:

| expression | raku | mutsu (before) |
| --- | --- | --- |
| `A.new(a=>5) === A.new(a=>5)` | `True` | `False` |
| `Set(A.new(a=>5), A.new(a=>5)).elems` | `1` | `2` |
| `Set(A.new(a=>5)).WHICH eq Set(A.new(a=>5)).WHICH` | `True` | `False` |
| `(A.new(a=>5), A.new(a=>6)).Set.WHICH eq ...` | `True` | `False` |
| `%h{A.new(a=>5)}` after storing under an equal key | the value | `(Any)` |
| `$s (|) $t` over such elements | 3 elems | 4 elems |
| `$s (&) $t` | 1 elem | 0 elems |
| `A.new(a=>5) (elem) $s` | `True` | `False` |

The ticket also asserted that raku requires a `ValueObjAt` return for value
semantics and that mutsu should reject a plain `Str`. That is wrong too:
Rakudo's `===` is `$a.WHICH eq $b.WHICH`, so a plain `Str`, an `ObjAt` and a
`ValueObjAt` all give value semantics identically (measured). mutsu now matches
that and simply stringifies whatever `WHICH` returns.

## Why the fix is not "thread the interpreter through the keying layer"

Running a user `WHICH` means running Raku code, which needs the interpreter.
But the identity consumers are deliberately interpreter-free: `value_which_key`
and `values_identical` are plain functions, and the keying layer beneath them
(`builtins/quanthash_coerce.rs`, `runtime/ops_set.rs`, `runtime/utils/set_ops.rs`
and friends — about 126 call sites across 25 files) is pure by design, which is
what lets the same code back both the VM fast path and the interpreter path.
Threading `&mut Interpreter` through all of it would have inverted that layering
for a rare feature, and would have meant calling arbitrary user code from inside
a half-built store's borrow.

So the identity travels on the object instead. `InstanceAttrs` gained a
`which_memo` slot (shared through the same `Arc` as the attribute cell, so every
alias and any reblessed view sees it). The interpreter computes the user's
answer at the points where it *is* in hand — always before any store borrow is
taken — and deposits it there; the pure layer reads it and falls back to the
per-object id when it is absent, which is exactly the right answer for a class
that does not override `WHICH`.

`runtime/which_identity.rs` holds the new `Interpreter::warm_which_identity`
(and a `which_key` convenience that warms then keys). It is gated on
`has_user_method(class, "WHICH")`, so a class without the override costs an MRO
probe and nothing else. A thread-local flag stops the obvious recursion: the
`.WHICH` dispatch entry point warms its receiver, and warming *is* a `.WHICH`
call.

Deposit points: quanthash coercion (both the `Set(...)` function form and the
`.Set`/`.Bag`/`.Mix` method forms, native and interpreter paths), the `===` /
`!==` opcodes, the `.WHICH` method dispatch (which doubles as the refresh point
for an object whose attributes changed), object-hash store / fetch / `:exists` /
`:delete`, and QuantHash membership (`(elem)`). Set operators need no deposit of
their own: they combine stores whose keys were already computed correctly.

## Pinned

`t/user-which-identity.t` — 42 assertions covering every row above plus the
controls that must *not* change: a class with no `WHICH` override keeps object
identity (two distinct instances are not `===`, stay separate Set elements, and
are distinct object-hash keys), while the same object is still `===` to itself.
The whole file passes unmodified under rakudo as well as mutsu.

## Found nearby, filed separately

Two identity divergences turned up in the neighbourhood sweep that are
independent of user `WHICH` (they reproduce with no user class at all), so they
are recorded as their own tickets rather than folded in here:
`todo/tickets/pair-which-is-object-identity-not-content.md` and
`todo/tickets/list-and-array-which-should-be-object-identity.md`.
