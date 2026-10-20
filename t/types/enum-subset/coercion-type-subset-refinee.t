use Test;

# A coercion type whose target is a subset (`my subset S of N where ...;
# my S() $x`) coerces through the subset's refinee: a value already in the
# subset stays, any other goes through the refinee's own coercion
# (`N.COERCE`, `Int()`), and a result outside the subset is
# X::Coerce::Impossible. mutsu looked for a coercion method on the subset
# itself and found none. PDF's `t/indobj-name.t`
# (`my subset MyName of PDF::COS::Name where ...; my MyName() $baz = Baz`,
# #12907). Expected values are the `raku` (v2026.09) output.

plan 6;

role N { multi method COERCE(Str:D $s) { $s but N } }
my subset S of N where 'Foo' | 'Bar';

my S() $x = 'Foo';
ok $x ~~ N, "a Str coerces through the refinee role's COERCE";
is ~$x, 'Foo', '... keeping its value';

enum « :Bar<Bar> »;
my S() $y = Bar;
ok $y ~~ N, 'a Str-valued enum value coerces too';

throws-like { my S() $z = 'Qux' }, X::Coerce::Impossible,
    'a coerced value outside the subset is an impossible coercion';

my subset P of Int where * > 0;
my P() $p = '5';
is $p, 5, 'a builtin refinee coerces through its own coercer';
throws-like { my P() $q = '-1' }, X::Coerce::Impossible,
    '... and a result outside the subset is impossible';
