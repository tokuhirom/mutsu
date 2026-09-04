# A `proto`/`multi sub` declared in a class or module body is lexical to it

A plain `sub` declared in a `class`/`module` body is lexical to that body: the
package's own routines see it, and it neither collides with nor shadows a
same-named mainline sub. mutsu got that right for a single `sub` and wrong for a
`proto`+`multi` family, in two independent ways.

All measured against Rakudo v2026.06 on 2026-09-04.

## Symptom 1 — a class-body family was unreachable from the class's own methods

```raku
class C {
    proto sub foo($) {*}
    multi sub foo(Int $x) { "in-class" }
    method m() { foo(5) }
}
say C.m();    # raku: in-class   mutsu: Unknown function: foo
```

Add a mainline `foo` and the method silently called *that* one instead.

**Cause.** Method dispatch sets `current_package` to the owner class only when
`has_class_scoped_subs` says the class body declared subs, and that fact comes
from a tail probe in `run_class_body` that tested
`functions.contains_key("C::foo")` — the **exact** key. A `multi sub` does not
register under that key: `insert_multi_overload` keys its candidates
`C::foo/1:Int` (plus `__mN` for same-signature siblings). So the probe never
fired, `current_package` stayed `GLOBAL`, and `bare_name_packages()` inside the
method never reached `C`. Confirmed with `rust-gdb` — the search list was
literally `["GLOBAL"]`. The probe now accepts the multi key shape as well.

## Symptom 2 — a module-body family was *merged* with the mainline one

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
```

The same signature, twice, from two packages.

**Cause.** The bare-name candidate gathers built one prefix list over *every*
package in `bare_name_packages()` and pooled all the matches into a single
ranking. That is right only while no enclosing scope declares its own `proto`:
raku gives a `proto` a **fresh** candidate list, and a `multi` declared with no
proto in scope extends the innermost visible proto's list instead.

`Interpreter::candidate_search_packages` now truncates the outward walk after
the innermost package that declares its own `proto` for the name, and
`resolve_function_with_types` and `resolve_all_matching_candidates` use it. A
package that has candidates but **no** proto of its own does not truncate, so the
extend-the-outer-family case keeps merging exactly as before — that case was
already correct and is now pinned.

## Pin

`t/package-body-routine-family-is-lexical.t` (13 tests), passing verbatim under
both `mutsu` and `raku`: the single-sub baseline, a class-body family reachable
from a method, a class-body family shadowing a mainline sub, the module-body
twin, nested packages each keeping their own family, both no-proto-of-its-own
extension shapes, and a class with no family of its own still seeing the
mainline one.

Validated with `make test` and a full local `make roast` in addition to CI.

## Left open

`module M { our proto sub foo($) {*}; our multi sub foo(Int $x) {…} }` is still
rejected ("Cannot declare individual multi candidates in 'our' scope"). That is a
*registration*-order defect in the CHECK-time inline-package prepass, not a
resolution one; it is filed as
`todo/tickets/our-multi-in-a-package-body-cannot-see-its-own-our-proto.md` with
two prototyped fixes and why each is wrong as written.
