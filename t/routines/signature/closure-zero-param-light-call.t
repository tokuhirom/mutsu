use Test;

# A closure with no parameters at all (`-> { ... }`, `sub () { ... }`) runs on
# the light closure path at arity 0; a call that passes arguments must still be
# rejected exactly as the general binder rejects it (#15737).

plan 10;

my &pt = -> { 41 + 1 };
my $sum = 0;
$sum += pt() for ^100;
is $sum, 4200, 'a zero-parameter pointy block called many times';

sub run-it(&code) { my $n = 0; $n += code() for ^10; $n }
is run-it(-> { 3 }), 30, 'called through a &code parameter';

my $outer = 5;
my &reads = -> { $outer * 2 };
is reads(), 10, 'it reads a captured variable';
$outer = 7;
is reads(), 14, 'and sees a later write to it';

dies-ok { pt(1) }, 'an argument to a zero-parameter pointy block is rejected';
is (try pt(1)) // $!.message, 'Too many positionals passed; expected 0 arguments but got 1',
    'with the binder\'s message';

my &an = sub () { 'anon' };
is an(), 'anon', 'a zero-parameter anonymous sub';
is (try an(1)) // $!.message, 'Too many positionals passed; expected 0 arguments but got 1',
    'rejects an argument the same way';

dies-ok { pt(:x) }, 'a named argument is rejected';

my @calls;
my &side = -> { @calls.push(1); @calls.elems };
side() for ^3;
is @calls.elems, 3, 'a body with a side effect runs each time';
