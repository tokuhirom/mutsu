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
- `UInt.new(...)` fell through to the same leaking fallback instead of delegating to
  its base type the way every other method call on a subset already does
  (`constraint_is_subset`/`dispatch_nominal_base`). This blocked any parametric role
  instantiated over `UInt` — including the builtin `Rational[NuT]` prelude — from
  ever constructing its typed attribute; `class P does Rational[UInt] {}; P.new(1,3)`
  now correctly answers `0.333333` instead of dying.

(#9795)
