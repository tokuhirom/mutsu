use Test;

# `$x.&f(args)` is the call `f($x, args)`: same sub, same multi pick, the
# invocant binds as the first positional (and as its container for `is rw`),
# and a visible `my &f` or an enclosing lexical sub keeps its meaning.
plan 12;

sub double($n) { $n * 2 }
sub add($a, $b) { $a + $b }
sub bump($n is rw) { $n++ }
multi sub kind(Int $v) { 'int' }
multi sub kind(Str $v) { 'str' }

my $x = 5;
is $x.&double, 10, 'no parentheses';
is $x.&add(3), 8, 'extra positional';
is 7.&double, 14, 'literal invocant';
is "ab".&kind, 'str', 'multi picks the candidate for the invocant';
is 4.&kind, 'int', 'and the other one';

my $n = 1;
$n.&bump;
is $n, 2, 'an is-rw first parameter writes back to the invocant variable';

my &local = -> $v { "local($v)" };
is $x.&local, 'local(5)', 'a visible my &f is what .&f calls';

{
    sub double($n) { "inner" ~ $n }
    is $x.&double, 'inner5', 'a nearer sub shadows the outer one';
}
is $x.&double, 10, 'and the outer one is back afterwards';

my @out;
for 1..3 { @out.push: $_.&double }
is-deeply @out, [2, 4, 6], 'inside a loop';

is (1, 2, 3).&elems, 3, 'a core routine through .&';
sub opt($v, :$by = 1) { $v * $by }
is $x.&opt(:by(3)), 15, 'a named argument after the invocant';
