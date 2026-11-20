use Test;

# `||`, `//`, `or` and their reductions keep the deciding operand as the
# value and evaluate nothing after it (the chains compile to a peeking jump
# and one Pop per link).

plan 22;

is (0 || '' || 5), 5, '|| chain: last operand when none is true';
is (0 || 'x' || 5), 'x', '|| chain: first true operand';
is (Nil // Any // 3), 3, '// chain: last when none is defined';
is (Nil // 0 // 3), 0, '// keeps a defined false value';
ok (0 || '') === '', '|| of two false operands is the last one';
is ([||] 0, 0, 7), 7, '[||] reduction';
is ([||] 0, ''), '', '[||] of false operands is the last';
is ([//] Nil, Any, 4), 4, '[//] reduction';
is ([//] Nil, 0, 4), 0, '[//] first defined';

my $calls = 0;
sub hit($v) { $calls++; $v }
is (hit(1) || hit(2)), 1, 'true left operand is the result';
is $calls, 1, '... and the right operand is not evaluated';
$calls = 0;
is (hit(Nil) // hit(2) // hit(3)), 2, '// stops at the first defined';
is $calls, 2, '... after evaluating exactly two operands';

my $r = (0 or 9);
is $r, 9, 'or';

sub is-space(Str $ch --> Bool:D) {
    $ch eq ' ' || $ch eq "\n" || $ch eq "\t" || $ch eq "\f" || $ch eq "\r";
}
ok is-space("\t"), 'a long || chain finds a middle match';
nok is-space('x'), 'and is False when nothing matches';
ok (is-space($_) for ' ', "\n", "\r").all, 'every link reachable';

# A Failure on the left is handled by the test, as before.
my $f = (sub { fail 'nope' })() || 'fallback';
is $f, 'fallback', 'a Failure is false and handled by ||';
my $g = (sub { fail 'nope' })() // 'default';
is $g, 'default', 'a Failure is undefined and handled by //';

my $x = 0;
$x ||= 4;
is $x, 4, '||= assigns when false';
my $y;
$y //= 5;
is $y, 5, '//= assigns when undefined';
is (False || Nil // 6), 6, 'mixed || and //';
