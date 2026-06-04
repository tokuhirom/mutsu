use Test;

# Decoupling pin (docs/vm-dual-store.md Slice 5 step 2, ANALYSIS.md §1.2): the
# compiled-function light call path no longer writes a *slot-only* positional
# param (one read via GetLocal, not in needs_env_sync, with no program-wide
# reflective by-name access) into the interpreter's shared `env`. The param
# lives purely in the VM's `locals`. This must not change any observable
# behaviour.

plan 16;

# Pure recursion: slot-only param, never mirrored into env.
sub fib($n) { $n < 2 ?? $n !! fib($n - 1) + fib($n - 2) }
is fib(10), 55, 'recursive slot-only param';
is fib(20), 6765, 'deeper recursion';

# Accumulator recursion (two slot-only params).
sub acc($n, $a) { $n == 0 ?? $a !! acc($n - 1, $a + $n) }
is acc(100, 0), 5050, 'two-param tail-style recursion';

# Nil-valued param must still be readable (GetLocal treats a Nil slot as
# possibly-undeclared and verifies via env, so Nil params are still mirrored).
sub id($x) { $x }
is id(Nil).raku, 'Nil', 'Nil param read back correctly';
is id(42), 42, 'Int param read back';
cmp-ok id(Int), '===', Int, 'type-object param read back';

# Return-type-checked sub with Nil (regression: return.t).
sub return-Int($x --> Int) { $x }
is return-Int(Nil), Nil, 'Nil through Int return typecheck';
is return-Int(42), 42, 'Int through Int return typecheck';

# Shadowing: a callee param must not be clobbered by a same-named outer var.
my $n = 5;
sub shadow($n) { $n }
is shadow(10), 10, 'param shadows same-named outer var (no env clobber)';

# Reflective access keeps params mirrored: EVAL reads the param by name.
sub eval-param($n) { EVAL('$n + 1') }
is eval-param(5), 6, 'EVAL reads param by name';

# Closure capturing a param keeps it env-synced (needs_env_sync).
sub make-adder($base) { -> $x { $x + $base } }
my $add5 = make-adder(5);
is $add5(3), 8, 'closure captures param';
is $add5(10), 15, 'closure capture stable across calls';

# Multi-dispatch on slot-only params.
multi mtype(Int $x) { "int:$x" }
multi mtype(Str $x) { "str:$x" }
is mtype(7), "int:7", 'multi dispatch Int param';
is mtype("hi"), "str:hi", 'multi dispatch Str param';

# A gather block compiles its body inline and snapshots the whole env by name;
# the param it reads ($num) must stay env-synced even though the gather body is
# not in closure_compiled_codes (regression: S02-types/lazy-lists.t).
sub make-lazy-list($num) { gather { take $_ for 0 ..^ $num }.lazy }
is make-lazy-list(4).List.join(','), '0,1,2,3', 'gather body reads slot-only param via env snapshot';
is make-lazy-list(10).first(*.is-prime), 2, 'lazy gather over param stays correct';
