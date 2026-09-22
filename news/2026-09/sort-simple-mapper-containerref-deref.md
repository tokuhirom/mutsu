# .sort's simple-mapper fast path derefs a ContainerRef element before extracting its key

`.sort(*.elems)` / `.sort({ .elems })` -- the "simple mapper" Schwartzian
fast path for a bare 0-arg method call on the topic -- silently stopped
sorting when an element was itself a `ContainerRef` (e.g. an inner Array
itemized by being pushed into an outer Array, exactly what
`@outer.push(@inner)` does).

Found via the `Graph` ecosystem distribution's `weakly-connected-components`,
whose components are built exactly this way (`@components.push(@component)`)
and then `.sort(*.elems)`ed. Investigation showed the real bug is **not** a
hash-iteration-order mismatch against rakudo as originally suspected (#9009)
-- mutsu's own output was internally non-deterministic across separate
process runs of the *same* program (varying with the unspecified hash
iteration order that happens to decide the pre-sort discovery order),
because the sort itself was silently a no-op.

Root cause: `VmSortCaller::call_method` (`src/vm/vm_native_sort.rs`)
dispatched the key-extraction method directly on the un-dereferenced
`ContainerRef`, which a generic dispatch fallback answered as `1` for every
element regardless of its real size -- so every Schwartzian key compared
equal and `Vec::sort_by`'s stability left the input order untouched. Every
other read path (`.map(*.elems)`, a hyper method call `>>.elems`, a plain
`for` loop) already treats the same `ContainerRef` as transparent, so only
this one fast path diverged.

Fixed by calling `recv.deref_container()` before dispatch in
`VmSortCaller::call_method`, mirroring the transparent `VarRef` deref
`call_method_with_values` already does for the identical class of problem.
Pinned by `t/collections/transform/sort-simple-mapper-container-elements.t`,
alongside the existing
`t/collections/transform/sort-inline-comparator-container-elements.t` (which
pins the analogous 2-arg comparator path against the same
container-promotion mechanism, there via `.grep`).

Closes #9009.
