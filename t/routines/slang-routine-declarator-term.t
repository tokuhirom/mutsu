use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's `routine-declarator:sym<...>` production is also tried where a term
# starts (ADR-15762 slice 2, #13370): `my $f = lam { ... }`.

plan 3;

use AnonRoutineSlang;

my $f = lam { 7 };
is $f(), 7, 'the production builds an anonymous sub in term position';
isa-ok $f, Sub, 'it is a Sub';
is (lam { 'x' })(), 'x', 'inside parentheses too';
