use v6;
use Test;

# `-> ($a)` is a one-parameter sub-signature, as on rakudo (#18397): the
# parenthesised group destructures even with a single element, and a group
# followed by more parameters (`-> ($a, $b), $c`) is one destructured parameter
# among others.

plan 9;

my $f = -> ($a) { $a.raku };
is $f((5,)), '5', 'a lone sub-signature unpacks a one-element list';
nok (try $f((1, 2))).defined, 'a one-element sub-signature rejects two elements';
throws-like { $f(21) }, X::Cannot::Capture, 'an Int cannot be unpacked';

is ((1,), (2,)).map(-> ($x) { $x }).join(','), '1,2', '.map with a lone sub-signature';

my $g = -> ($a, ($b, $c)), $d { "$a $b $c $d" };
is $g((1, (2, 3)), 4), '1 2 3 4', 'nested group followed by a parameter';

my $h = -> ($a, $b), $c { "$a$b$c" };
is $h((1, 2), 3), '123', 'group followed by a parameter';

my $seen;
if (7,) -> ($v) { $seen = $v }
is $seen, 7, 'if with a lone sub-signature';

my @pairs;
for ((1, 2), (3, 4)) -> ($a, $b) { @pairs.push("$a$b") }
is @pairs.join(' '), '12 34', 'for with a destructuring signature';

use experimental :rakuast;
ok Q[-> ($a) { }].AST.statements[0].expression.signature.raku.contains('sub-signature'),
    'the RakuAST view carries the sub-signature';
