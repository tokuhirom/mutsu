use v6;
use Test;

# A user-declared infix:<+> must be honored by JIT-compiled code: the Tier B
# inline Add fast path is guarded by the process-wide USER_INFIX_DECLS
# counter (vm_jit.rs), so once this declaration registers, every JIT'd `+`
# routes through the interpreter helper, which dispatches to the override.
# Kept separate from t/jit-tier-b-arith.t: the counter is process-wide and
# monotonic, so this file must not share a process with fast-path pins.

plan 2;

multi sub infix:<+>(Str $a, Str $b) { $a ~ "|" ~ $b }

sub cat2($a, $b) { $a + $b }

my $sink;
for ^300 { $sink = cat2("x", "y") }

is cat2("x", "y"), "x|y", 'user infix:<+> dispatches from hot (JIT-eligible) code';
is cat2(3, 4), 7, 'Int + Int still native when the override does not match';
