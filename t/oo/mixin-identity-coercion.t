use Test;

# A coercion to a type the value already is returns the value itself in
# rakudo (`method Int(Int:D:) { self }`), so a `but`-mixed value keeps its
# mixin through it, method or prefix operator alike. A coercion that builds a
# new value, an allomorph's, and a role's own override do not.

plan 14;

role R { method foo { 'foo' } }
my $i = 5 but R;
is $i.Int.foo, 'foo', 'Int.Int';
is $i.Numeric.foo, 'foo', 'Int.Numeric';
is $i.Real.foo, 'foo', 'Int.Real';
is (+$i).foo, 'foo', 'prefix +';
nok $i.Num.^can('foo'), 'Int.Num builds a new value';
is (1.5 but R).Rat.foo, 'foo', 'Rat.Rat';
is (1e0 but R).Num.foo, 'foo', 'Num.Num';
is ('s' but R).Str.foo, 'foo', 'Str.Str';
is (~('s' but R)).foo, 'foo', 'prefix ~';
is (?(True but R)).foo, 'foo', 'prefix ?';

my $five = 5 but 'five';
is ~(+$five), 'five', 'a Str mixin survives prefix +';
is (+<42>).^name, 'Int', 'an allomorph numifies to its plain number';
is (5 but role { method Int { 9 } }).Int, 9, "a role's own Int wins";
is ~('a' but role { method Str { 'z' } }), 'z', "a role's own Str wins for prefix ~";
