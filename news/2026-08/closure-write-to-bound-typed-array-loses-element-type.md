# A closure write to a `:=`-bound native-typed array no longer loses its element type

```raku
my int @src = 1, 2, 3;
my @a := @src;
sub inner(\values) { @a = values }
inner((4, 5, 6));
say @a.WHAT;   # raku: (array[int])   mutsu (before): (Array)
```

A same-scope reassignment (`@a = (4,5,6)` in the same block that declared the
bind) already worked: the `SetLocal` path snapshots the slot's old backing
`Gc<ArrayData>` and copies the new items into it in place, which naturally
keeps the container's `value_type`/`declared_type` metadata. But a write
reached **by name** — a nested closure/sub with no local slot for the free
variable, `our @a`, or (as of the previous fix in this series) a `for`-loop
multi-param bind — compiles to `SetGlobal`, a completely different code path
that had no equivalent preservation: it called `coerce_typed_container_assignment`
directly and used its result as-is, and that function's native-element-type
branch built a fresh `ArrayData::new(coerced_items)` with no `value_type`/
`declared_type` stamped on it at all.

There was already a helper built for exactly this shape —
`array_container_writethrough_value`, used by the `SetLocal` ContainerRef
writethrough path for `my @b := @a; @a = ...` — that coerces elements to the
declared/inherited type and then stamps the correct metadata via
`tag_container_metadata`. The `SetGlobal` path now routes through the same
helper instead of its own narrower, metadata-losing logic.

Fixing this surfaced a second bug in the same helper: it began by calling
`coerce_to_array` on its input unconditionally, but `coerce_to_array`'s
`Array`-input arm unconditionally rebuilds the array with kind
`ArrayKind::Array` — silently stripping an already-lazy tag `coerce_to_array`
just gave an infinite Range (`array[num] @arr; @arr = 0e0..Inf` inside a
closure), and the native-typed-array laziness check never got to see the tag
to raise `X::Cannot::Lazy`. Fixed by skipping the redundant re-coercion when
the input value is already array-shaped.

Pin: `t/for-multi-param-writethrough-metadata.t`.
