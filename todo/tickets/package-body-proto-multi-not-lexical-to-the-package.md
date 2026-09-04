# A `proto`/`multi sub` declared in a class or module body is not lexical to it

A plain `sub` declared in a `class`/`module` body is lexical to that body: the
package's own methods see it, and it neither collides with nor shadows a
same-named mainline sub. mutsu gets this right for a single `sub` and wrong for a
`proto`+`multi` family.

All measured against Rakudo v2026.06 on 2026-09-04.

## Correct today (the single-sub baseline)

```raku
sub foo($x) { "mainline" }
class C {
    sub foo($x) { "in-class" }
    method m() { foo(5) }
}
say C.m();    # raku: in-class   mutsu: in-class
say foo(5);   # raku: mainline   mutsu: mainline
```

The `module M { sub foo(...); our sub go() { foo(5) } }` twin is correct too.

## Wrong: class body

```raku
class C {
    proto sub foo($) {*}
    multi sub foo(Int $x) { "in-class" }
    method m() { foo(5) }
}
say C.m();
# raku:  in-class
# mutsu: Unknown function: foo   (in sub m)
```

A class-body `proto`+`multi` family is not reachable from the class's own methods
at all, even with no mainline `foo` in the picture. Add a mainline `foo` and the
method silently calls *that* one instead:

```raku
sub foo($x) { "mainline" }
class C {
    proto sub foo($) {*}
    multi sub foo(Int $x) { "in-class" }
    method m() { foo(5) }
}
say C.m();    # raku: in-class   mutsu: mainline
say foo(5);   # raku: mainline   mutsu: mainline
```

## Wrong: module body

```raku
module M {
    proto sub foo($) {*}
    multi sub foo(Int $x) { "in-module" }
    our sub go() { foo(5) }
}
proto sub foo($) {*}
multi sub foo(Int $x) { "mainline" }
say M::go();
# raku:  in-module
# mutsu: Ambiguous call to foo(Int); these signatures all match: (Int $x), (Int $x)
say foo(5);   # raku: mainline
```

Without the mainline `foo` this works (`M::go()` returns `in-module`), so the two
candidate sets are being *merged* across the package boundary rather than one
shadowing the other. The registry keys differ (`M::foo/1:Int` vs
`GLOBAL::foo/1:Int`), so this is a resolution-side problem, not a registration
collision: multi resolution for a bare name reached from inside `M` gathers
candidates from both `M::` and `GLOBAL::` and ranks them together.

## Why this is not the lexical-shadow fix

`news/2026-09/proto-multi-lexical-scope.md` fixed the *registration* side: an inner
lexical scope may now declare its own `proto` without a spurious
`X::Redeclaration`, and the block-scope registry snapshot/restore brings the outer
one back. That machinery is keyed on `block_scope_depth` / `__lexical_hoist` and on
the `Package::name` registry key, and it works because both declarations share one
key. A package body registers under a *different* key, so nothing shadows and the
defect surfaces in `resolve_function_with_types` / the multi-candidate gather
instead — a different code path with a different fix.

Start from `src/runtime/dispatch_resolve.rs` and the multi-candidate gather in
`src/runtime/registration_sub.rs` (`insert_multi_overload`'s `Pkg::name/arity:types`
keys), and work out how a bare-name call inside package `P` should rank `P::`
candidates against `GLOBAL::` ones. The single-sub path already answers that
question correctly for one key; the multi gather does not.
