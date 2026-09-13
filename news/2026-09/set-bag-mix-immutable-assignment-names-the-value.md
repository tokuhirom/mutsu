# Assigning into an immutable Set/Bag/Mix names the value, not just the type

```raku
my $s = Set.new(<a b>);
$s<a> = False;
```

`raku` says:

```
Cannot modify an immutable Set (Set(a b))
```

mutsu said:

```
Cannot modify an immutable value (Set)
```

The keyed-store path for `Set`/`Bag`/`Mix` (`container.with_set_mut(...)`,
`with_bag_mut(...)`, `with_mix_mut(...)` in `vm_var_assign_index_named.rs`)
raised `RuntimeError::assignment_ro(Some("Set"))` — a generic constructor
that only knows a type-name string, not the value itself. The correct
constructor, `assignment_ro_value(value: Value)`, already existed right next
to it and renders both the type name and the value's `.gist` the way rakudo
does; it just wasn't reachable from this path because `container` was
already mutably borrowed by the `with_*_mut` closure at the point the error
is raised, so the value couldn't be read again from inside it.

Fixed by cloning the container's value into a local binding *before* each
`with_set_mut`/`with_bag_mut`/`with_mix_mut` call and moving that snapshot
into the closure, so the closure's read-only-check branch can call
`assignment_ro_value` with the real value instead of a bare type name.

`t/collections/set-bag-mix/quanthash-immutable-ro.t` gained three assertions
pinning the message shape for direct keyed assignment (`$b<a> = ...`) on
each of `Bag`, `Set`, and `Mix` — the existing assertions in that file cover
`:delete`, `DELETE-KEY`, and `ASSIGN-KEY`, which already went through
`assignment_ro_value` via a different call path and were unaffected.

Part of the survey in
[#7556](https://github.com/tokuhirom/mutsu/issues/7556).
