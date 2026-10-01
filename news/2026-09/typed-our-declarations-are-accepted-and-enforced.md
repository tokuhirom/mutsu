# Typed `our` declarations compile again, and their constraint is enforced

`our Int $x`, `our Int @a`, `our Int %h`, `our Int ($a, $b)` and `our Int $.x`
were refused with "Cannot put a type constraint on an 'our'-scoped variable".
That refusal was added on 2026-09-07 (`aa6e5e8fe`, `07943ef43`; see
`news/2026-09/state-typed-declarations-hoist-and-our-typed-declarations-are-refused.md`
and `news/2026-09/our-typed-destructuring-and-attribute-declarations-refused.md`)
on the strength of a measurement against Rakudo 2026.07, and its rationale was
that a package variable is reachable by its qualified name from anywhere, so
there is nowhere to enforce a constraint. Rakudo v2026.09 accepts every one of
those spellings, in the default compiler and in `RAKUDO_RAKUAST=1` alike, and
enforces the type on the container, so the refusal had become a compatibility
bug (#10410).

The three refusal sites (the `VarDecl` arm of the compiler, the destructuring
list and the class attribute in the parser) are gone. Removing them was nearly
enough, because the name-keyed constraint machinery already enforced a typed
`our` inside its own scope. Two gaps needed real changes, both because the
constraint did not travel with the variable:

- A typed `our` scalar was stored twice, once in its lexical slot and once under
  the package-qualified name, and only the lexical name knew the type, so
  `$Pkg::v = "a"` and `$GLOBAL::x = "a"` were accepted. A typed, non-native
  `our` scalar now takes the same shared-cell path an untyped one already did,
  and `DeclareOurScalar` registers the declared constraint on that cell.
- A typed `our @a` / `our %h` published a second, coerced copy of its raw
  initializer under the qualified name, so `@Pkg::a.WHAT` was `Array` and
  `%Pkg::h.WHAT` was `Hash`, and the element constraint was lost. They now
  publish the typed container that `SetLocal` just built, so the qualified name
  reads `Array[Int]` / `Hash[Int]` and `@Pkg::a.push("x")` is refused.

`t/types/state-and-our-typed-declarations.t` now pins the accepted spellings,
the type-object default, enforcement by name, through `$Pkg::x` and through
`@Pkg::a`, and the typed container identities. The whole file passes under
`raku` v2026.09 unchanged.

What is still different, tracked separately: an element store through a
qualified name (`@Pkg::a[5] = "x"`, `%Pkg::h<b> = "x"`), an assignment through
the stash or a symbolic name (`Pkg::<$v> = "a"`), and a `:=` alias of `$Pkg::v`
still bypass the constraint, because the element-store ops read the constraint
from the variable's name rather than from the container.
