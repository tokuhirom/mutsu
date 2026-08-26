# `enum Foo does Role (...)` composes for real, and a composed `ACCEPTS` now drives `~~`

```raku
role Weird { multi method ACCEPTS(Int:D $v) { True } }
enum Flags does Weird (A => 1, B => 2);
say 5 ~~ A;   # raku: True; mutsu: False, plus two spurious warnings
```

The ticket recorded two symptoms. They had two separate root causes, and the
second turned out not to be enum-specific at all.

## 1. The `does` clause was never parsed

`parse_enum_decl_body_with_type` had a loop for `is <trait>` clauses but nothing
for `does`. With `does Weird` left unconsumed, the enum's `(...)`/`<...>` body was
never read as its value list: the declaration registered an enum with **no
values**, and the leftover `does Weird (A => 1, B => 2)` was parsed as an
ordinary expression statement — which is exactly where the ticket's spurious
`Useless use of "=>" in expression "A => 1" in sink context` warnings came from.
`A` and `B` were then plain barewords, so `A.^name` answered `Str`.

The trait loop now accepts `is` and `does` interchangeably and repeatedly, in
either order (`enum E does A does B is export <x y>`), and a `does R[Int]`
parameterization keeps its argument list in the recorded name. The roles ride on
`Stmt::EnumDecl` to registration, where `compose_roles_onto_enum`
(`src/runtime/methods_enum_roles.rs`) records them — with their transitive role
parents — in the same `class_composed_roles` registry a class's `does` uses. An
enum has no `ClassDef`, but role membership is keyed by type *name*, so the store
is shared rather than duplicated.

Two consumers were then taught about it: `type_matching.rs`'s `Enum` arm, so an
enum *value* type-checks as doing the role (`A ~~ Weird`; the enum type object
already did, via the `Package` path), and `methods_call_dispatch.rs`, which tries
`dispatch_enum_role_method` ahead of the built-in enum methods so a composed
method wins over the built-in of the same name — which is precisely what makes a
role-supplied `ACCEPTS` an override rather than a shadowed sibling.

## 2. The smartmatch `ACCEPTS` protocol only looked at `Instance` matchers

`vm_smart_match` dispatched `$obj.ACCEPTS($x)` only when the right-hand side was
a `ValueView::Instance` whose class declared `ACCEPTS`. Everything else fell into
`pure_smart_match`, which compares underlying values and can never reach an
override. That is not an enum bug — the same gap made a *runtime* mixin wrong:

```raku
my $matcher = 5 but Weird;
say 7 ~~ $matcher;   # raku: True; mutsu (before): False
```

The check is now "does the matcher carry a user-visible `ACCEPTS`", answered per
value shape: a class instance as before, a `Mixin` through the existing
`mixin_composes_method`, and an enum value or enum type object through the new
`enum_composes_role_method`. Both the enum case and the `but`-mixin case now
answer `True`.

## Verification

`t/enum-role-and-enumhow.t` covers value declaration under `does`, role methods
on both the values and the type object, role type-checks, several `does` clauses,
`is export` on either side of `does`, the `ACCEPTS` override on both the enum and
a runtime mixin, and that a plain enum keeps the built-in `Enumeration.ACCEPTS`
(compare-by-value) semantics. It passes verbatim under both `raku` and mutsu, as
does a wider side-by-side probe of the same ground.

One deliberate divergence: mutsu's `Flags.^roles` reports the composed role,
where Rakudo's `EnumHOW.^roles` answers an empty list (its role list lives in
`^role_typecheck_list`). Nothing in the suite depends on the empty answer, and
reporting the role is the more useful of the two.
