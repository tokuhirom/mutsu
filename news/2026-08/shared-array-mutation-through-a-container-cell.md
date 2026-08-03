# A shared array push seeded its store from an undereferenced cell, and emptied the array

Once a program spawns a thread, `shared_vars_active` stays on for the rest of
the process and every plain lexical `@a.push` funnels through the name-keyed
`__mutsu_atomic_arr::` store, so concurrent appends serialize instead of
clobbering each other's stale snapshots.

The first mutation seeds that store from the array's current contents:

```rust
let seed = match shared.get(arr_name).or_else(|| self.env.get(arr_name)).map(Value::view) {
    Some(ValueView::Array(elems, _)) => elems.as_ref().clone(),
    _ => crate::value::ArrayData::default(),   // <- empty
};
```

The env binding is not always a bare `Array`. Every lexical a closure captures
is boxed into a `ContainerRef` cell — which is the shape a *module* file-scope
`my @a` has as seen from the module's own subs. `ValueView::ContainerRef(..)`
matches neither arm, so the atomic entry was seeded **empty** and everything the
array already held was dropped. The write-back then did
`self.env.insert(arr_name, updated)`, replacing the cell with a bare `Array` and
detaching every other holder of the same container.

Both halves are fixed: the seed dereferences the binding, and the write-back
goes *through* the cell when there is one, so the container keeps its identity.

## How it showed up

Under the real `Test.rakumod` the array in question is `@vars`, the subtest
stack. A test file that spawns a thread part-way through a subtest lost the
outer frame, so the enclosing `subtest`'s `_pop_vars` died:

```
Cannot pop from an empty Array
  in sub _pop_vars at Test.rakumod line 900
  in sub subtest at Test.rakumod line 438
  in block <unit> at roast/S02-types/capture.t line 301
```

`roast/S02-types/capture.t` is exactly that shape — its
`types whose .Capture behaves like Mu.Capture` subtest reaches
`(start {sleep .5}).&has-nameds`, and every `subtest` after that point pushed
into a store that had thrown the outer frame away. The file now runs its 46
assertions clean under `MUTSU_REAL_TEST=1`.

Reduced to 20 lines with no `Test` involved at all: a module with a file-scope
`my @vars`, a sub that pushes to it and a sub that pops, called around a
`start`/`await` — the push after the spawn silently did nothing.

Pin: `t/shared-array-mutate-keeps-container-cell.t`, which covers the module
shape, the in-process closure-captured shape, and the nesting depth, and fails
3 of its 6 assertions without the fix.
