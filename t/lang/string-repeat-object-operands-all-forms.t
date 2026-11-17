use v6;
use Test;

# Infix `x` repeats its left operand's `.Str` by its right operand's `.Int`,
# whichever form reaches it: the operator, the routine (`infix:<x>(...)`), and
# the built-in fallback of a unit that declares its own `infix:<x>` candidate,
# also when that unit runs as `EVAL($source.AST)`. Checked against rakudo.

plan 9;

class Q { method Str { 'q' }; method Int { 3 } }
my $q = Q.new;

is $q x 2, 'qq', 'the operator stringifies the left operand by .Str';
is 2 x $q, '222', 'the operator counts the right operand by .Int';
is infix:<x>($q, 2), 'qq', 'the routine form stringifies by .Str';
is infix:<x>(2, $q), '222', 'the routine form counts by .Int';

{
    multi sub infix:<x>(Q $a, Q $b) { 'custom' }
    is $q x $q, 'custom', 'a user candidate answers what it accepts';
    is $q x 2, 'qq', 'the built-in fallback stringifies by .Str';
    is 2 x $q, '222', 'the built-in fallback counts by .Int';
}

use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
my $source = q:to/CODE/;
    class R { method Str { 'r' }; method Int { 2 } }
    multi sub infix:<x>(R $a, R $b) { 'custom' }
    my $r = R.new;
    ($r x 2, 3 x $r)
    CODE
my @got = EVAL($source.AST);
is @got[0], 'rr', 'a lowered unit\'s fallback stringifies by .Str';
is @got[1], '33', 'a lowered unit\'s fallback counts by .Int';
