# A chained subscript store fires a `Proxy` element's STORE

A `Proxy` bound to an element mediates its own store. `@a[0] := Proxy.new(...)`
installs the `Proxy` as the element's container, so a later `@a[0] = 7` must
fire that `Proxy`'s `STORE` rather than overwrite the container with a plain
value — ADR-0040 §9 seen from the destination side. The single-subscript op
has implemented that since the ADR landed: `exec_index_assign_expr_named_op_seeded_inner`
probes the addressed slot with `existing_element_container` before any of its
fast paths or its slow path runs, because every one of them ends in a plain
`items_mut()[i] = ...` / `insert(k, v)` that would replace the `Proxy`.

The **chained** spelling did not. `exec_index_assign_expr_nested_op_body` had a
`Proxy` arm for the user-object descent (`$q<foo>[0] = v`) and one for an
`is rw` accessor's returned location, but nothing looked for a `Proxy` sitting
in the slot a plain `@a[$i][$j]` addresses. The store walked straight past it
into `Value::assign_element_slot` / `Value::hash_insert_through` and clobbered
the binding, so the backing variable never moved:

```raku
my @p;
@p[0] = [0];
my $backing = 0;
@p[0][0] := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v * 10 });
@p[0][0] = 7;
say $backing;      # rakudo: 70    mutsu: 0
```

All four inner/outer container combinations diverged the same way
(`@a[$i][$j]`, `%h<a><b>`, `%h<a>[$i]`, `@a[$i]<k>`), since the two descent
arms they split across both end in an unmediated slot write.

## The fix

One probe, `nested_element_store_proxy`, placed above every descent arm — the
same "one hook above the dispatch" shape the single-subscript op uses, rather
than a `Proxy` check bolted onto each arm. It walks the root one step through
the inner subscript with the op's own `subscript_peek_step`, then hands the
inner container and the outer subscript to the single-subscript path's probe,
`existing_element_container`, which is now `pub(super)` instead of private.
Reusing that probe is what makes both `:=` bind spellings work: a `Proxy`
installed directly (`@a[0][0] := Proxy.new(...)`) and one reached through a
variable whose own container is that `Proxy` (`@a[0][0] := $p`) sit at
different depths, and the probe already unwraps the alias cells between them.

A `:=` bind is excluded, exactly as at the single-subscript site: a bind is
installing a container, not storing through whatever the slot happens to hold,
so re-binding an already-`Proxy` element replaces it instead of calling its
`STORE`. The store site mirrors the twin's, `assign_proxy_lvalue` followed by
the `apply_pending_rw_writeback` drain, so a `STORE` that writes a caller
lexical by name lands.

Since the probe runs on the *pre-existing* body rather than on the chained
store's fast lane (#8069) — the lane already declines on a `Proxy` destination
— the divergence was neither caused nor hidden by that lane, and the fix sits
where the lane falls through to.

## Pinned

`t/vm/binding/bind-chained-proxy-store.t`, ten assertions: the four container
combinations, the alias-cell bind spelling, that a re-bind installs rather than
stores, that an aggregate RHS reaches `STORE` itemized exactly as the
single-subscript twin's does, and that an ordinary chained store is
undisturbed. The whole file passes unchanged under rakudo.

Closes #8965.
