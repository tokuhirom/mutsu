use Test;

# A plain-variable argument reaches a call site wrapped in a `VarRef` so an
# `is rw` parameter can bind the caller's container. `normalize_call_args_for_
# target` strips that wrapper for an unregistered name, but deliberately keeps
# it when a user routine of the same name is registered -- so declaring ANY
# `multi sub abs(...)` left every fall-through-to-core call holding a wrapped
# argument, which the pure native function table does not know and answered
# from its catch-all arm: `abs($n)` gave 0 and `is-prime($n)` gave False.
#
# Math::NumberTheory declares `multi sub is-prime(Complex:D)`, which made its
# trial division skip every prime divisor and run to sqrt(20!) -- the file did
# not finish in 300s (issue #7995).

plan 12;

class Tagged { has $.v }

{
    multi sub abs(Tagged:D $p) { 'tagged' } #OK shadow
    my $d = -3;
    is abs($d), 3, 'core abs answers a variable argument through the shadowed name';
    is abs(-3), 3, 'core abs still answers a literal argument';
    my @a = -7, ;
    is abs(@a[0]), 7, 'core abs answers an array-element argument';
    is abs(Tagged.new(v => 1)), 'tagged', 'the user candidate still wins when it matches';
}

{
    multi sub is-prime(Complex:D $p) { 'complex' } #OK shadow
    my $d = 3;
    is is-prime($d), True, 'core is-prime answers a variable argument';
    is is-prime(4), False, 'core is-prime still answers a literal argument';
    my $e = 9;
    is is-prime($e), False, 'core is-prime answers a composite variable argument';
}

{
    multi sub uc(Tagged:D $p) { 'tagged' } #OK shadow
    my $s = 'ab';
    is uc($s), 'AB', 'core uc answers a variable argument';
}

# Two-argument builtins take the same route.
{
    multi sub substr(Tagged:D $p) { 'tagged' } #OK shadow
    my $t = 'abcdef';
    my $from = 1;
    is substr($t, $from, 3), 'bcd', 'core substr answers variable arguments';
}

# A captured outer lexical arrives as a VarRef around a shared cell.
{
    multi sub abs(Tagged:U $p) { 'type-object' } #OK shadow
    sub outer {
        my $x = -5;
        my $inner = sub { abs($x) };
        $x = -11;
        $inner()
    }
    is outer(), 11, 'core abs answers a captured-cell argument at its current value';
}

# The shadowing routine keeps winning for the arguments it does accept.
{
    multi sub sqrt(Tagged:D $s) { 'tagged-sqrt' } #OK shadow
    my $n = 16;
    is sqrt($n), 4e0, 'core sqrt answers a variable argument';
    is sqrt(Tagged.new(v => 2)), 'tagged-sqrt', 'the user candidate wins on its own type';
}
