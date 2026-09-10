use Test;

# Decoupling pin (docs/vm-dual-store.md Slice 5, ANALYSIS.md §1.2): the VM no
# longer mirrors a frame's slot-only locals (those read via GetLocal, not
# referenced by name and not captured by a closure) into the interpreter's
# name-keyed `env`. This shrinks the VM<->Interpreter shared-state surface.
# It must NOT change any observable behaviour -- in particular, every path that
# reads a variable BY NAME (EVAL, symbolic deref, closures, phasers, loops)
# must still see current values, because those readers keep the variable
# env-synced (via needs_env_sync / the conservative loop+block fallback).

plan 12;

# Pure recursion (slot-only param): the case the gate optimizes.
sub fib($n) { $n < 2 ?? $n !! fib($n-1) + fib($n-2) }
is fib(10), 55, 'recursive slot-only param computes correctly';

# Reassigned slot-only local read by name via EVAL inside a function.
sub eval-local() { my $x = 5; $x = 10; $x = $x + 1; EVAL('$x * 2') }
is eval-local(), 22, 'EVAL sees reassigned slot-only local by name';

# Slot-only local read by symbolic dereference.
sub sym-local() { my $w = 7; $w = 99; my $nm = "w"; ::($nm) }
is sym-local(), 99, 'symbolic deref sees slot-only local by name';

# Loop body mutating an outer local (conservative full-mirror path).
sub loop-sum() { my $s = 0; for 1..5 { $s = $s + $_ }; $s }
is loop-sum(), 15, 'loop body mutation of outer local';

# Loop body + reflective read afterwards.
sub loop-eval() { my $s = 0; for 1..5 { $s = $s + $_ }; EVAL('$s') }
is loop-eval(), 15, 'EVAL after loop sees mutated local';

# FIRST/NEXT/LAST loop phasers thread state through control temps.
my $seq = "";
for 1..3 { FIRST { $seq ~= "F" }; NEXT { $seq ~= "N" }; $seq ~= "X"; LAST { $seq ~= "L" } }
is $seq, "FXNXNXNL", 'FIRST/NEXT/LAST loop phasers preserved';

# Closure capturing and mutating an outer local (needs_env_sync path).
sub make-counter() { my $n = 0; my $inc = -> { $n = $n + 1; $n }; ($inc(), $inc(), $inc()) }
is-deeply make-counter().list, (1, 2, 3).list, 'closure capture+mutate of outer local';

# ENTER/LEAVE block phasers.
my $out = "";
{ ENTER { $out ~= "E" }; LEAVE { $out ~= "L" }; $out ~= "B" }
is $out, "EBL", 'ENTER/LEAVE block phasers';

# Nested function calls preserve each frame's locals.
sub outer-fn($a) { my $b = $a * 2; inner-fn($a) + $b }
sub inner-fn($x) { $x + 1 }
is outer-fn(10), 31, 'nested calls preserve caller locals across the call';

# while loop with a mutated condition local.
sub count-down() { my $c = 5; my $acc = 0; while $c > 0 { $acc = $acc + $c; $c = $c - 1 }; $acc }
is count-down(), 15, 'while loop with mutated locals';

# State variable in a recursive sub (slot interplay with state storage).
sub with-state() { state $calls = 0; $calls = $calls + 1; $calls }
with-state(); with-state();
is with-state(), 3, 'state var accumulates across calls';

# Recursion with a local mutated between recursive calls.
sub acc-rec($n, $a) { $n == 0 ?? $a !! acc-rec($n - 1, $a + $n) }
is acc-rec(5, 0), 15, 'tail-style recursion with accumulator';
