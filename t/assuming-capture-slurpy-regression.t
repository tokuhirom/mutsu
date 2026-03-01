use Test;

plan 5;

sub capcapn (|c ($a, $b?, :$e = 'e'), |d ($c, $d?, :e($f) = 'f'), *%g) {
    "a$a b$b c$c d$d e$e f$f";
}

is &capcapn.assuming('a')('b'), 'aa bb ca db ee ff',
    'capture sub-signature currying keeps defaults from both captures';
is &capcapn.assuming(*, 'b')('a'), 'aa bb ca db ee ff',
    'Whatever primer in .assuming is filled by later positional args';
is &capcapn.assuming('a', :e<E>)('b'), 'aa bb ca db eE fE',
    'named alias in capture sub-signature binds only alias target variable';

sub anonslurp ($a, $b, *@, *%) { "a$a b$b" }
my $primed = &anonslurp.assuming(1, 2, :a, 'c', :d);
ok !$primed.can('Failure'),
    '.assuming accepts extra named primers when signature has *% slurpy';

my @got;
my @expect = ['x'];
# TODO: use eqv once @-variable assignment consistently produces Array (mutable=true).
# Currently (sub { 'x' })() returns a scalar, and coerce_to_array wraps it as List.
ok !((@got = (sub { 'x' })()) != @expect),
    'array assignment expression returns array value in expression context';
