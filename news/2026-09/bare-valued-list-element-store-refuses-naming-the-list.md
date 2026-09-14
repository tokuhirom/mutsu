# A chained store into a bare-valued List element is refused, not clobbered

```raku
my @a = (1, 2, 3), 4;
@a[0][0] = 9;
```

`raku` refuses:

```
Cannot modify an immutable List ((1 2 3))
```

mutsu silently clobbered the List's own backing storage instead, turning
`@a` into `[(9 2 3) 4]`.

`@a[0]` is a `List`, and a `List` is not a container of its own — writing
into one of its elements is only legitimate when that specific element has
already been promoted to a shared cell (the shape `take-rw` builds:
`t/collections/lazy-seq/take-rw-shared-cell.t`). Refusing every store into a
`List`/`ItemList` slot *by its kind* was tried before (see the "how the
surviving rows differ" section of
[#7556](https://github.com/tokuhirom/mutsu/issues/7556)) and regressed that
exact file, because rakudo's refusal is decided by the specific *element*
the final subscript reaches, not by the List's kind at the point an outer
subscript descends into it.

So the fix has to sit at the innermost element write, once the reached
element's own shape is known — not at `subscript_descent_refusal_at`'s
descent-time check (which deliberately still treats every `ArrayKind` as
descendable, so a `take-rw`-built List keeps writing through). Both of
`vm_var_assign_index_named.rs`'s chained-store engines needed it: the
2-level named chain (`exec_index_assign_expr_nested_op_body`) and the 3+
level deep-nested walk (`exec_index_assign_deep_nested_op_body`) each had
their own `arr[i].with_array_mut(|inner_arr, _kind| ...)` call that
discarded the array's kind and unconditionally overwrote the element slot.
Both now check: if the array is `List`/`ItemList` and the specific element
is not already a `ContainerRef` cell, refuse naming the *List itself*
(matching rakudo's message, which stays the same regardless of which index
or whether it's out of range) instead of writing through. A `:=`-bind and a
real (mutable) `Array`/`Shaped` element store are both unaffected.

`t/collections/subscript/chained-subscript-store-refuses-defined-value.t`
gained 4 assertions: the 2-level case, an out-of-range inner index, the
3+-level (hash-rooted) twin, and a real-Array control case confirming
ordinary element stores still write through.

Part of the survey in
[#7556](https://github.com/tokuhirom/mutsu/issues/7556).
