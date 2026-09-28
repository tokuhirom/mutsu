# The internal "fallback disabled" dispatcher message no longer leaks to user code

Several unrelated method-dispatch failures reported mutsu's own internal wording,
`Unknown method value dispatch (fallback disabled): <name>`, instead of the message
Rakudo produces for the same failure:

- `G.parse("a")` on a grammar with no `TOP` (or `G.parse("a", :rule<y>)` naming an
  undefined rule) named the outer `.parse`/`.parsefile` call instead of the actually
  missing start rule; it now says `No such method 'TOP' for invocant of type 'G'`
  (or `'y'`).
- `Foo::Bar.new` where `Foo` was never declared as any kind of package auto-vivifies
  as a bare `Package` value at term-resolution time, so `.new` dispatch is the first
  point that can tell an undeclared qualified symbol apart from a genuinely missing
  method on a real type; it now says `Could not find symbol '&Bar' in 'GLOBAL::Foo'`,
  matching Rakudo's global-symbol-lookup error.
- `.^ver`/`.^auth`/`.^api`/`.^trusts` on a bare `package` (which composes
  `PackageHOW` and never gains these, "absent by design" per
  `S12-introspection/meta-class.t`) already threw the right exception type but the
  leaking message, plus a nonsensical "Did you mean 'put'?" suggestion computed
  against the pool of ordinary object methods. A new `RuntimeError::meta_method_not_found`
  reports these MOP-method failures without that misleading suggestion.
- `class P does Rational[UInt] {}; P.new(1,3)` died instead of answering `0.333333`
  (a documented, legal parameterization per raku-doc's `Type/Rational.rakudoc`). The
  builtin `Rational[NuT]` role prelude's `method new` unconditionally called
  `NuT.new(...)`/`DeT.new(...)` to build the typed numerator/denominator, but `UInt`
  is a subset (`subset UInt of Int where * >= 0`) and Rakudo never permits `.new()`
  on a subset at all — `UInt.new` itself still correctly throws
  (`Cannot instantiate a subtype`, per `S32-num/int.t` and the `S02-types/subset-*.t`
  family), same as any user `subset ... of ...`. The prelude now checks
  `NuT.HOW ~~ Metamodel::SubsetHOW` and only calls `.new()` in the genuine-class case
  (still required for e.g. `does Rational[Foo, Foo]` where `Foo` is a user
  `class Foo is Int {}`); for a subset, the already-checked value from the parameter
  binding is used directly.

(#9795)
