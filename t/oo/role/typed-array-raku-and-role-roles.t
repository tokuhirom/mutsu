use Test;

plan 6;

# `.raku` of a typed container built from an Array subclass names `Array[T]`
# and carries no `$(...)` itemization (identical under raku).
class P is Array { }
my $x = P[Int].new(1, 2);
is $x.raku, 'Array[Int].new(1, 2)', 'P[Int].new(...).raku';
is P[Int].new(3).raku, 'Array[Int].new(3)', 'bare P[Int].new(...).raku';
my $z = P.new(1, 2);
is $z.raku, '$[1, 2]', 'untyped subclass instance in a scalar';

# `.^roles` of a specialized parametric role substitutes its type parameter.
role R[::T] does Positional[T] { method of() { T } }
role S[::T] does R[T] { }
is R[Str].^roles.map(*.^name).join(','), 'Positional[Str]', 'R[Str].^roles';
is S[Int].^roles.map(*.^name).join(','), 'R[Int],Positional[Int]', 'S[Int].^roles';
is Array::Typed[Str].^roles.map(*.^name).join(','), 'Positional[Str]', 'Array::Typed[Str].^roles';
