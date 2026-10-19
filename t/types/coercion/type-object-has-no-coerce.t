use Test;

# A type object has no `coerce` method: coercion is the `CoercionHOW`'s
# (`Int(Str).HOW.coerce`), and a class may declare its own `coerce`
# (ADR-11276 §9.192; every answer checked against Rakudo).

plan 4;

throws-like { Int.coerce("3") }, X::Method::NotFound, 'Int.coerce is not a method';
throws-like { Str.coerce(3) }, X::Method::NotFound, 'Str.coerce is not a method';
is Int(Str).HOW.coerce(Int(Str), "3"), 3, 'the coercion type HOW coerces';

class C { method coerce($x) { "own $x" } }
is C.coerce(5), 'own 5', "a class's own coerce";
