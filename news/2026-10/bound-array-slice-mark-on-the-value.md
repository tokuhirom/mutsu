# A bound array slice is marked on its value, and writes through from closures

`my @s := @a[1, 2]` makes `@s` a fixed-arity view over two of `@a`'s elements.
A later `@s = …` writes through them. mutsu recorded "this name is a bound
slice" under a name-keyed `__mutsu_bound_array_slice::@s` env marker of the
declaring frame. So only that frame's `SetLocal` honoured it: a closure or
routine assigning to the captured `@s` (`{ @s = 8, 9 }()`, `sub h { @s = 7, 7 }`)
replaced the binding and left `@a` untouched, where Rakudo writes through.

The mark now lives on the bound value: a new `ArrayData::bound_slice` flag, set
when the bind-time slice promotion builds the cell list (or when a `:=` bind
meets a list of element cells). It cannot outlive the binding or leak to a
later same-named variable, so the per-declaration clear and the env latch that
gated the probes are gone. `SetGlobal` writes through like `SetLocal` does.

The three copies of the write-through loop are now one helper,
`store_through_bound_slice`. The marker's `MetaNs::BoundArraySlice` namespace
and the `BindingDesc::bound_slice_sym` field are retired. This is another
ADR-0097 slice (#8087).
