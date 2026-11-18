use Test;

# A mutating call of a code object takes colon arguments like a method name
# does: `$x .= &f: 3` is `$x = $x.&f(3)`. From CSS::Specification::Compiler's
# `$regex-body .= &quantified: $_ with $quant`, which failed to parse.
# Expected values checked against raku.

plan 5;

sub q($a, $b = 2) { $a * $b }

my $x = 5;
$x .= &q: 3;
is $x, 15, '.= &f: arg';

my $y = 4;
$y .= &q;
is $y, 8, '.= &f with no arguments';

my $z = 7;
$z .= &q(10);
is $z, 70, '.= &f(arg)';

my &named = &q;
my $w = 2;
$w .= &named: 21;
is $w, 42, '.= through a code variable with colon arguments';

my $v = 6;
$v .= &q: 5 with 1;
is $v, 30, 'a statement modifier ends the colon argument list';
