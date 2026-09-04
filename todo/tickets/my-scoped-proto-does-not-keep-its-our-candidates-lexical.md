# A `my`-scoped proto does not keep its `our` candidates out of the package stash

```
raku  -e 'module M { proto sub f($) {*}; our multi sub f(Int $x) { "f" } }; say M::f(1)'
Could not find symbol '&f' in 'M'
mutsu -e '...same...'
f
```

Measured 2026-09-04 against `raku` v2026.06.

The PROTO's scope decides the whole multi's visibility: a bare `proto sub` is
`my`-scoped, so the multi is lexical to the package body and `M::f` is not a
package symbol — even though every candidate says `our`. mutsu resolves it.

The positive counterpart is already right: with `our proto sub f`, `M::f(1)`
works (pinned by `t/our-multi-in-a-package-body.t`), and
`register_proto_decl`'s `is_our` branch calls `mark_our_scoped_package_item`
for exactly that reason. What is missing is the negative half — a NON-`our`
proto should mark the key `my`-scoped and win over the candidates' own `our`,
the mirror image of the comment already in `register_proto_decl`:

> `our proto sub f(|) {*}` makes the whole multi a package symbol. Its
> candidates are declared bare (`multi sub f(...)`) and each of those marks
> `Pkg::f` my-scoped, which would hide the routine from the package stash
> however the proto was declared — so record the our-visibility up front and
> let it win.

Check `mark_my_scoped_package_item`'s interaction with the candidates' `our`
before assuming it is a one-liner: the candidates run AFTER the proto, so
whichever mark is applied last wins unless the proto's decision is recorded as
authoritative the way `is_our` already is.

## Provenance

Split out while landing `news/2026-09/our-multi-in-a-package-body.md`
(2026-09-04). Before that change mutsu rejected the whole declaration, so the
lookup was unreachable; accepting it (as rakudo does) exposed this.
