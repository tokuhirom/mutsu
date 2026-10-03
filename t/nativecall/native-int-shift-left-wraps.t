use v6;
use Test;

# From SION (zef distribution): a base64 decoder accumulates bits in a native
# `int` with `$acc +< 6`, relying on the shift wrapping at 64 bits.
plan 4;

my int $acc = 0;
$acc = ($acc +< 6) + 5 for 1..12;
is $acc, 5856109229749064005, '+< on a native int wraps instead of promoting';

my int $one = 1;
is $one +< 70, 64, 'shift count is taken modulo 64';
my int $big = 4611686018427387904;
is $big +< 1, -9223372036854775808, 'shift into the sign bit';
my Int $boxed = 4611686018427387904;
is $boxed +< 1, 9223372036854775808, 'a boxed Int still promotes';
