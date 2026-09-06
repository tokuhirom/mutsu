# `.VAR` on an itemized list element reports the inner type, not `Scalar`

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/structures.rakudoc:26`),
re-verified against raku v2026.07 the same day.

## Repro

```raku
(1, 2, 3, $(4, 5))[3].VAR.^name.say;
# raku:  Scalar
# mutsu: List
```

The `$(...)` itemization is exactly what puts a `Scalar` container around the
inner list, and `.VAR` is the one introspection that is supposed to report it —
that is the point of the doc example, which is illustrating "itemization is what
makes a list one element".

## Relationship to ADR-0064

ADR-0064 made `.VAR` synthesise a container descriptor from the contained value
rather than requiring a real container to exist. That is what lets
`$obj.attr.VAR.^name` answer `Scalar` without every attribute being boxed. This
row looks like the same synthesis declining for an element that *is* genuinely
itemized: the value is `Scalar(List)`, and `.VAR` is answering for the inner
`List` instead of for the `Scalar` wrapper.

`ADR-0067`'s `native-method-cannot-return-an-lvalue-container` file carries a
standing warning worth repeating here: **do not use `.VAR` to test whether a
container arrived** — `S.VAR.WHAT` reports `Scalar` whether or not one did.
This ticket is the mirror image (it reports the inner type where a wrapper
really is present), so a fix must not be validated by `.VAR` alone; check the
itemization survives `.raku`, `.elems` and `.WHAT` too.

## Where to look

The `.VAR` dispatch (`methods_mut_dispatch.rs`'s VAR arm and
`src/vm/vm_call_method_ops.rs`'s `method == "VAR"` early return) and how it
treats a `ValueView::Scalar` wrapper around an aggregate.

## Neighbourhood to check when fixing

`$[1,2]` (itemized Array) as well as `$(1,2)`; `.VAR.^name` on a plain
(non-itemized) list element, which must stay the element's own type;
`@a[0].VAR.^name` where `@a`'s element was itemized by a reference push
(ADR-0040 slice 1 already gives that shape a `Scalar(ContainerRef(cell))`);
`%h<k>.VAR.^name`; and `.VAR.WHAT` / `.VAR.^name` right after each other, which
ADR-0064 handles as a documented pair.
