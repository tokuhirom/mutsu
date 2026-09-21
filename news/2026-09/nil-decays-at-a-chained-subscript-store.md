# `Nil` decays to the container default at a chained subscript store

ADR-0049's rule is that an `Array`/`Hash` element is a `Scalar`, and a `Scalar` cannot hold `Nil`:
assigning `Nil` to one restores the container's default. Slice 4 wired the single-subscript store
(`@a[0] = Nil`) onto that decision point. The **chained** store never reached it.

```raku
my @d is default(42);
@d[0] = [1, 2];
@d[0][0] = Nil;
say @d[0][0].raku;   # rakudo: Any    mutsu (before): Nil
```

Every descent arm in `exec_index_assign_expr_nested_op_body` and its 3+-level twin
`exec_index_assign_deep_nested_op_body` (`src/vm/vm_var_assign_index_named.rs`) wrote the rvalue
through `Value::assign_element_slot` / `Value::hash_insert_through` exactly as given, so a raw `Nil`
landed in the slot — the one value a `=` store can never leave there. The same divergence showed up
at every chained shape: hash-of-hash, hash-of-array, array-of-hash, the deep chain, and an
autovivified row.

## The decision point, reached once

Both ops now decay above their descent arms, through a new
`Interpreter::nested_store_nil_default` (`src/vm/vm_var_assign_nil_decay.rs`), rather than
re-deriving the rule per arm.

The subtlety is *whose* default. At a chain the owning container is the **row** the earlier
subscripts reach, not the root variable — and a row is an ordinary `Array`/`Hash` that does not
inherit the root's `is default(...)`. So rakudo answers `Any` above, not `42`, and the helper
consults only the row's own embedded state: the `typed_container_default` ladder
`assign_store_nil_default` itself ends on (an explicit `is default(...)`, then declared element-type
metadata, then the untyped `Any`). It deliberately omits that function's name-keyed
`var_default`/`var_type_constraint` fallback, which is right for the single-subscript store — where
the target name *is* the owning container — and wrong here.

The row is found with the same read-only `subscript_peek_step` walk the `*-1` subscript resolution
already runs, and comes back absent exactly when the arms below are about to walk-create it untyped.

Two shapes are excluded on purpose. The hook sits *under* each op's `Proxy` arm, because a `Proxy`
element is not a `Scalar` and its `STORE` takes the raw `Nil`; and a `:=` bind replaces the element
container rather than storing into it, so `@a[0][0] := Nil` still reads `Nil`.

## Measured, not assumed

Every expectation was taken from real `raku`, and mutsu now matches all of them:

| | raku | mutsu (before) |
| --- | --- | --- |
| `my @d is default(42); @d[0] = [1,2]; @d[0][0] = Nil` | `Any` | `Nil` |
| `@t[0] = Array[Int].new(1,2); @t[0][0] = Nil` | `Int` | `Nil` |
| `@f[0] := my @inner is default(7); @f[0][0] = Nil` | `7` | `Nil` |
| `%g<a> = [1,2]; %g<a>[0] = Nil` | `$[Any, 2]` | `$[Nil, 2]` |
| `@z[0][0][0] = 1; @z[0][0][0] = Nil` | `Any` | `Nil` |
| `my @u; @u[0][0] = Nil; @u.raku` | `[[Any],]` | `[[Nil],]` |
| `my $r = (@e[0][0] = Nil)` | `Any` | `Nil` |
| `@bind[0][0] := Nil` | `Nil` | `Nil` |

Pinned by `t/vm/binding/nil-decay-chained-element-store.t`, which passes unchanged under `raku`
itself as well as under mutsu. The `todo` this divergence carried in
`t/vm/binding/nested-element-store-fast-lane.t` (the fast-lane decline pin that surfaced it) is
removed, and ADR-0049's implementation status records the follow-up.
