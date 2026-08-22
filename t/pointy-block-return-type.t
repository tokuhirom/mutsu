use v6;
use Test;

# A pointy block may declare a return type (`-> $x --> Int { ... }`), and the
# constraint is checked on the block's return value just like a sub's.
#
# Regression: a *single*-parameter pointy block parsed to the internal `Lambda`
# node, which has nowhere to keep the return type, so the constraint was
# silently dropped (`-> $x --> Int { "s" }` happily returned a Str). The
# multi-parameter form already carried it.

plan 8;

my $one = -> $x --> Int { $x * 2 };
is $one(4), 8, 'a single-parameter pointy block with a matching return type works';
dies-ok { my $f = -> $x --> Int { "s" }; $f(1) },
    'a single-parameter pointy block enforces its return type';

my $two = -> $x, $y --> Int { $x + $y };
is $two(3, 4), 7, 'a two-parameter pointy block with a matching return type works';
dies-ok { my $f = -> $x, $y --> Int { "s" }; $f(1, 2) },
    'a two-parameter pointy block enforces its return type';

my $none = -> --> Int { 7 };
is $none(), 7, 'a parameter-less pointy block with a return type works';

# The declared return type is visible through the block's signature.
is (-> $x --> Int { $x }).signature.returns.^name, 'Int',
    'the return type is reachable through .signature.returns';

# A typed parameter plus a return type compose.
my $typed = -> Int $x --> Str { "v$x" };
is $typed(3), 'v3', 'a typed parameter composes with a return type';

# A pointy block without a return type is unconstrained, as before.
my $plain = -> $x { "s" };
is $plain(1), 's', 'a pointy block with no return type is unconstrained';
