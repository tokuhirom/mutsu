# `@a[0]:p` yields a snapshot Pair, so `.value = X` depends on finding the array in `%*ENV`-style env

Minimal repro — the *second* block is what breaks the *first*:

```raku
{ my @a = <A B>; my $b = { (@a[0]:p).value = "x" }; $b(); say @a; }
{ my @a = <C D>; }
```

```
raku : [x B]
mutsu: X::Assignment::RO: cannot assign through .value on non-instance
```

Delete the second block and mutsu prints `[x B]`. Rename its `@a` to `@b` and
mutsu prints `[x B]`. Nothing about the second block runs before the failure —
it changes only how the *first* block is compiled.

## Why a later sibling block decides it

`--dump-bytecode` on the two programs differs in exactly one instruction for the
first block:

```
one block   two blocks
PushBlockFrame        BlockScope { pre_end: 1, …, is_bare_block: true }
```

A bare block that is followed by a sibling redeclaring the same name compiles to
`BlockScope`, and `@a` then lives in a **local slot** rather than in `self.env`.

That matters because `.value = X` on a subscript pair is still a tree-walk-era
heuristic. `@a[0]:p` builds `Value::value_pair(key, value)` from a *snapshot* of
the element (`builtins_multidim_subscript.rs:509` and `:636`), so the Pair holds
no link back to the array. `assign_method_lvalue_with_values`
(`methods_mut_method_lvalue.rs:434`…) then has to *find* the backing container,
and the way it finds it is to scan `self.env.values()` for an Array whose
element at that index is identical to the Pair's value
(`methods_mut_method_lvalue.rs:562-578`), rebuild it, and write it back by
identity. With `@a` in a local slot the scan matches nothing, control falls out
of the whole `method == "value"` arm, and the generic instance-attribute path at
the bottom of the function reports `cannot assign through .value on
non-instance` (confirmed under `rust-gdb`: the working program stops at the
array-rebuild branch, line 630; the failing one at line 1054).

## The fix is a live element container, not a wider search

The tempting patch is to make the scan look at `self.locals` too. That is the
wrong direction — it doubles down on a heuristic that is already wrong in other
ways (it silently declines when two env entries hold equal-looking arrays, and
it rebuilds the array instead of writing in place).

Raku's `:p` returns `0 => @a[0]` where the value **is** the element container, so
`.value = X` is an ordinary container write with no search at all. mutsu already
has that path: `assign_method_lvalue_with_values` handles
`ValueView::ContainerRef` at line 468 by locking the cell and assigning, type
constraint included. What is missing is a way to *obtain* a `ContainerRef` for
an array/hash element — there is no `array_element_cell`-style API today, and
element cells are exactly what ADR-0001 fuses with the GC campaign (layer 3a /
Track B element-cell-ification, `docs/adr/0001-gc-strategy-and-phasing.md`).

So this is a Track-B-shaped item, not a one-file fix. Related consumers that
would collapse onto the same mechanism once it exists:

- `for @a.pairs { .value = X }`, which today needs `topic_source_var` plus the
  same env scan (`methods_mut_method_lvalue.rs:501`).
- the QuantHash weight write-back next to it.
- `@a[0]:kv` — note `(@a[0]:kv)[1] = 'x'` works today, so the `:kv` list form
  reaches the array by a different route and would want auditing at the same
  time.

## Where it showed up

`t/subscript-adverbs.t` under the vendored upstream `Test` module
(`todo/tickets/vendor-real-test-module.md`). It is not a `Test` difference: the
real `lives-ok` takes the block as `Callable $code` and calls it, which is what
puts the write inside a closure, and the file's own second `{ my @a = ... }`
block is what flips the first one to `BlockScope`. Both halves reproduce with no
`Test` in sight, as above.
