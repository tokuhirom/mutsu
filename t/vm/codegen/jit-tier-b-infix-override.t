use v6;
use Test;

# A user-declared infix:<+> must be honored by JIT-compiled code: the Tier B
# inline Add fast path is guarded by the process-wide USER_INFIX_DECLS
# counter (vm_jit.rs), so once this declaration registers, every JIT'd `+`
# routes through the interpreter helper, which dispatches to the override.
# Kept separate from t/jit-tier-b-arith.t: the counter is process-wide and
# monotonic, so this file must not share a process with fast-path pins.
#
# The same guard covers the inline Sub and Mul paths: until #10111 only Add
# carried it, so a hot loop stopped calling a user `multi infix:<*>` (or
# `infix:<->`) on Int operands once the loop was JIT-compiled -- 99 calls out
# of 300 -- and answered with the native operator instead.

plan 5;

multi sub infix:<+>(Str $a, Str $b) { $a ~ "|" ~ $b }

sub cat2($a, $b) { $a + $b }

my $sink;
for ^300 { $sink = cat2("x", "y") }

is cat2("x", "y"), "x|y", 'user infix:<+> dispatches from hot (JIT-eligible) code';
is cat2(3, 4), 7, 'Int + Int still native when the override does not match';

{
    my %calls;
    multi sub infix:<*>(UInt $a, UInt $b) { %calls<mul>++; callsame() }
    multi sub infix:<->(UInt $a, UInt $b) { %calls<sub>++; callsame() }
    my $x = 5;
    for 1..300 { $x = $x * 1; $x = $x - 0 }
    is %calls<mul>, 300, 'user infix:<*> on Int operands runs on every iteration of a hot loop';
    is %calls<sub>, 300, 'user infix:<-> on Int operands runs on every iteration of a hot loop';
    is $x, 5, 'and the core candidate it defers to still answers';
}
