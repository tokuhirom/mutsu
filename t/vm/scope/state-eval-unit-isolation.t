use MONKEY-SEE-NO-EVAL;
use Test;

is EVAL(q[state $x = 1; $x]), 1, 'the first EVAL initializes its state';
is EVAL(q[state $x = 2; $x]), 2, 'another EVAL owns a different state declaration';
is EVAL(q[(state $x = 3)]), 3, 'an inline state declaration owns its compilation site';
is EVAL(q[(state $x = 4)]), 4, 'separate inline declarations do not share state';
is EVAL(q[state $outer = state $inner = 5; $outer]), 5,
    'a nested state initializer has its own compilation sites';
is EVAL(q[state $outer = state $inner = 6; $outer]), 6,
    'nested state declarations in another unit initialize independently';
is EVAL(q[sub f { state $x = 7; ++$x }; f(); f()]), 9,
    'repeated calls within a compiled routine retain state';
is EVAL(q[sub f { (state $x = 8)++; }; f(); f()]), 9,
    'an inline declaration retains state across repeated calls';
for ^2 {
    is EVAL(q[state $x = 1; ++$x]), 2,
        'the same source compiled again owns a fresh state declaration';
}
done-testing;
