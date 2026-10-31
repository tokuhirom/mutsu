use Test;

# A method's plain scalar parameter is read through its slot; the frame does
# not write it into the env (ADR-14633). Every by-name reader of that
# parameter still has to see it.

plan 7;

class C {
    method by-eval($x) { EVAL '$x + 1' }
    method by-my($x) { MY::<$x> }
    method by-symbolic($x) { ::('$x') }
    method by-closure($x) { my &c = { $x * 2 }; c() }
    method by-block($x) { do { $x ~ '!' } }
    method rw-param($x is rw) { $x = 9 }
    method two($a, $b) { EVAL '$a ~ $b' }
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
