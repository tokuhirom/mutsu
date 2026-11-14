# An exception inside `COERCE` reaches the caller as itself

When a target type's `COERCE` method threw, mutsu misreported it in two
ways. If the body called a method that does not exist (CSS::Font's
`COERCE` reaches `CSS::Properties`' `FALLBACK`, which dies with
X::Method::NotFound), any X::Method::NotFound was taken to mean `COERCE`
itself was missing. The error became "Impossible coercion ... no acceptable
coercion method found". Any other exception thrown through a coercing
parameter (`sub f(Foo:D() $x)`) was rewrapped as
X::TypeCheck::Binding::Parameter, and a unit-level `CATCH` ran twice, once
for each exception.

Only an X::Method::NotFound for `COERCE` itself now counts as "no such
coercion". A real exception from a parameter coercion propagates unchanged,
as on rakudo. The test is `t/exceptions/coerce-method-exception-propagates.t`.
