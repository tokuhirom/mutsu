use Test;

# A method's plain scalar parameter is read through its slot; the frame does
# not write it into the env (ADR-14633). Every by-name reader of that
# parameter still has to see it.

plan 13;

class C {
    method by-eval($x) { EVAL '$x + 1' }
    method by-my($x) { MY::<$x> }
    method by-symbolic($x) { ::('$x') }
    method by-closure($x) { my &c = { $x * 2 }; c() }
    method by-block($x) { do { $x ~ '!' } }
    method rw-param($x is rw) { $x = 9 }
    method two($a, $b) { EVAL '$a ~ $b' }
    has $!n = 0;
    # a method that writes an attribute takes the non-pure frame shape
    method write-attr($x) { $!n = $x; my &c = { $x + $!n }; c() }
    method slash($/) { $0 }
    method topic($_) { .uc }
    method no-params { 42 }
    method only-scalar($x) { $x * 2 }
    method aggregate(@xs) { @xs.push(9); @xs.elems }
}

my $o = C.new;
is $o.by-eval(41), 42, 'EVAL reads a scalar parameter';
is $o.by-my(5), 5, 'MY::<$x> reads a scalar parameter';
is $o.by-symbolic(6), 6, 'a symbolic lookup reads a scalar parameter';
is $o.by-closure(4), 8, 'a closure captures a scalar parameter';
is $o.by-block(3), '3!', 'a nested block reads a scalar parameter';
my $v = 1;
$o.rw-param($v);
is $v, 9, 'an is rw parameter still writes through to the caller';
is $o.two('a', 'b'), 'ab', 'EVAL reads two scalar parameters';
is $o.write-attr(5), 10, 'a parameter is read by a closure in a method that writes an attribute';
is $o.slash('ab' ~~ /(a)/), 'a', 'a $/ parameter is the match';
is $o.topic('ab'), 'AB', 'a $_ parameter is the topic';

# A frame with nothing to write into its env must not perturb it: calls with no
# parameters, with only scalar parameters and with an aggregate keep working,
# also when repeated and nested.
is $o.no-params + $o.no-params, 84, 'a method with no parameters can be called repeatedly';
is $o.only-scalar($o.only-scalar(3)), 12, 'scalar-only methods nest';
my @a = 1, 2;
is $o.aggregate(@a), 3, 'an aggregate parameter still shares the caller container';
