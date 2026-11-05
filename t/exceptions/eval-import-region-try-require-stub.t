use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

plan 4;

# `require Foo::Bar` declares a lexical stub for the name in the enclosing
# import layer. An EVAL unit that holds a `use` runs over such a layer, and a
# `try` block above it copies the tiers in between when it binds the stub, so
# ending the unit used to find the layer under an extra tier (a debug-build
# assertion failure). The unit must end cleanly and keep its result.

is EVAL(q[use Test; my $r = 1; try { require Zork::NotInstalled }; $r + 1]), 2,
    'a failed require in a try block does not break the EVAL unit that ends with it';

lives-ok { EVAL q[use Test; try { require Zork::NotInstalled }] },
    'the try block is the last statement of an EVAL unit holding a use';

lives-ok { EVAL q[use Test; try { my $x = 1; require Zork::NotInstalled }] },
    'same with a lexical declared in the try block';

is EVAL(q[use Test; my $e; try { require Zork::NotInstalled; CATCH { default { $e = .^name } } }; $e]),
    'X::CompUnit::UnsatisfiedDependency', 'the failure is still catchable';
