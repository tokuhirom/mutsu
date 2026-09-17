# Reference-pushed array element no longer iterates as one opaque item

`@a.push(@b)` (Raku's non-flattening `**@` slurpy) shares a `ContainerRef` cell between the source
`@b` and the pushed element, so a later mutation of `@b` still propagates through `@a[0]`, and
itemizes the pushed element so `@a[0].raku` reads `$[...]` (matching ADR-0040) while `@b.raku` on
its own stays bare.

That itemization was implemented as `Value::container_ref(cell).item()`. `Value::item()` has no
`ContainerRef` arm, so it fell into the generic "wrap in `Scalar`" fallback, producing
`Scalar(ContainerRef(cell))` — the container was buried two levels deep instead of one.
`Value::with_deref`/`deref_container` only unwrap a bare `ContainerRef`/`ContainerView` (a `Scalar`
falls through their match to `f(self)` unchanged), so this extra layer was invisible to them. A
`for @outer -> @row {...}` loop's `@`-sigil parameter binding derefs through the wrapper for
`.elems`/`.gist`/display (both of those go through a different, more permissive read path), but any
reader that iterates through the *same* deref chokepoint — a nested `for @row -> $x {...}`,
`@row.map(...)`, `@row.hyper(:batch(1)).map(...)` — saw only the outer `Scalar`/`ContainerRef` layer
and bound its block parameter to the WHOLE group as one item instead of flattening its elements.

This is exactly the shape zef's `Zef::Repository.candidates` uses: `push @plugins, @group; for
self.plugins -> @repo-group { @repo-group.hyper(:batch(1)).map: -> $repo { $repo.id } }`. It made
`mzef install <anything>` die immediately with `No such method 'id' for invocant of type 'Array'`,
before even searching a backend — see [#8609](https://github.com/tokuhirom/mutsu/issues/8609).

The fix itemizes the `ContainerRef` VALUE itself, via the `ContainerRefItemized` nan-box tag that
already existed and is used at several other binding sites (`vm_var_assign_coerce.rs`,
`runtime/types/binding_signature.rs`) but was, at this one call site, dead code. The cell's stored
content is untouched, so the source variable's own plain (non-itemized) binding to the same cell is
unaffected — only this one value's tag changes, and `with_deref` already special-cases
`ContainerRefItemized` correctly (it itemizes the value it hands back, without needing an outer
wrapper). `t/collections/array/push-array-reference.t` gained a regression case pinning both the
plain `for`-loop and the `.hyper(:batch(1)).map` shape.
