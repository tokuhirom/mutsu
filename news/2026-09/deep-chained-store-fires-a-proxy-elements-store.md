# The 3+-level chained store fires a `Proxy` element's STORE too

A `Proxy` bound to an element mediates its own store: `@a[0] = 7` fires the `Proxy`'s `STORE`
rather than replacing the container (ADR-0040 §9, seen from the destination side). #8965 brought
the **chained** spelling in line by adding one probe above every descent arm of
`exec_index_assign_expr_nested_op_body`.

That op only serves a two-level chain. Three or more subscripts compile to
`exec_index_assign_deep_nested_op_body`, which never got the probe:

```raku
my @a;
@a[0][0] = [0];
my $backing = 0;
@a[0][0][0] := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v * 10 });
@a[0][0][0] = 7;
say $backing;   # rakudo: 70    mutsu (before): 0
```

Its raw-pointer walk ends in a plain element-slot write, so the store overwrote the `Proxy` and
the backing variable never moved — the same failure #8965 described, one op over.

## One probe, any depth

Rather than copy the two-level hook, `nested_element_store_proxy` was generalized into
`chained_element_store_proxy`: it walks the root down every subscript *but the last* and then
probes the leaf slot with the single-subscript path's own `existing_element_container`. Both
chained-store ops call it, so there is one answer to "is there a `Proxy` at the destination"
rather than two. Neither caller allocates — the two-level op passes a one-element path via
`slice::from_ref`, the deep op passes `indices[..depth - 1]` directly.

The hook sits above the deep op's walk, and above ADR-0049's `Nil` decay: a `Proxy` is not a
`Scalar`, so its `STORE` takes the raw `Nil` rather than the container's default.

## A second bug the probe exposed: `bind_cell` is not "is this a bind"

Adding the hook under `bind_cell.is_none()` — the deep op's existing spelling for "not a `:=`" —
turned a re-bind into a store:

```raku
@p[0][0][0] := Proxy.new(FETCH => -> $ { $first  }, STORE => -> $, $v { $first  = $v * 10 });
@p[0][0][0] := Proxy.new(FETCH => -> $ { $second }, STORE => -> $, $v { $second = $v - 1  });
@p[0][0][0] = 9;                      # rakudo: $second is 8, $first stays 0
```

`bind_cell` is built from `bind_source`, and a bind whose RHS is a **literal** carries no source
variable, so both stay `None` while the statement is still very much a bind. The second `:=` was
therefore read as a store through the `Proxy` already in the slot, and the new one never landed.

Whether a statement is a bind is the marker's presence (`__mutsu_bind_index_value`), not the
cell's — which the op's own junction re-dispatch already knew, keying on the marker for exactly
this reason. An `is_bind_value` flag is now taken once at the unwrap site and used by both hooks
(the `Proxy` probe and the `Nil` decay). Without it the `Nil` decay had the same latent hole: a
literal `@a[0][0][0] := Nil` would have decayed instead of binding.

## Verification

All shapes were measured against real `raku`: three and four levels, all-hash and mixed
positional/associative chains, both `:=` bind spellings (literal and through an alias cell), the
re-bind above, an aggregate RHS reaching `STORE` itemized, a `Nil` reaching it undecayed, and a
plain deep store left alone. `t/vm/binding/bind-chained-proxy-store.t` grew from 10 to 21
assertions and passes unchanged under `raku` itself.

The `todo` #8965 left in `t/vm/binding/nested-element-store-fast-lane.t` — that file did not exist
on `main` when the fix landed, so the marker could not be removed then — is gone, alongside
#8966's. Both blocks stay as live pins.
