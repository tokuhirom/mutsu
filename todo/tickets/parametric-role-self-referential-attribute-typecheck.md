# A parameterized role with a self-referential attribute type fails a spurious type-check, with a malformed error message

Discovered via the doc-diff harness on `raku-doc/doc/Language/objects.rakudoc` (around line
1397).

## Repro

```
role Box[::Type] {
    has Box[Type] $.child;
    has Type $.val;
}
my $b = Box[Int].new(val => 1);
say $b.^name;
```

- raku: `Box[Int]`
- mutsu: `Type check failed in assignment to $!child; expected Box[Int]::Box[Int] but got
  Box[Type] (Box[Type])`

Note the malformed doubled type name in the error text itself (`Box[Int]::Box[Int]`) — a strong
clue that the parametric-role instantiation is being applied twice (or concatenated instead of
substituted) when computing the expected type for the self-referential `has Box[Type] $.child`
attribute. Also note `$.child` is never assigned in the repro (it defaults to its type), so this
type-check shouldn't even fire for an unset attribute.

## Root cause guess

Two candidate bugs, possibly both present:
1. The default (unset) value of a typed attribute is being type-checked against the
   *parameterized* attribute type at all — Raku shouldn't type-check an attribute against its
   declared type when it's simply left at its default/undefined value.
2. The parametric-role attribute type `Box[Type]` (self-referential, `Type` bound to the *same*
   role's own parameter) resolves to a malformed/duplicated type name (`Box[Int]::Box[Int]`)
   instead of `Box[Int]`, suggesting the role-parameter substitution runs twice or concatenates
   the outer instantiation's name onto the inner one.

## Affected files (starting point)

- `src/runtime/class.rs` — parametric role instantiation / type substitution
- Wherever attribute type-checking happens on `.new` (look for where the malformed
  `ClassName::ClassName` string could be constructed — likely a role-parameterization
  name-building helper called twice)

## Suggested next step

Grep for how a parametric role's type name is built during instantiation
(`Box[Int]`-style formatting) and trace why the self-referential `has Box[Type] $.child`
attribute's expected-type string ends up doubled.
