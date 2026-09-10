# Recursion through a `start` block must return the right answer.
#
# `clone_for_thread` used to seed EVERY lexical into `shared_vars`, a flat
# name-keyed map. It cannot represent two concurrently-live bindings of the same
# name -- which is exactly what a recursive frame chain is -- so every frame's
# `$n` collapsed to one value. The block's own captures are owned per binding by
# the closure machinery (`start` compiles its block as escaping), so the flat map
# was a second, lossy mechanism running in parallel with a working one and
# silently overwriting its correct answer. See PLAN.md §6 and
# docs/probes/pool-recursive-start.raku.

use Test;

plan 8;

# await INSIDE the start block: the 2nd call used to give b|3|3|3.
{
    sub f($n) { start { $n <= 0 ?? "b" !! await(f($n - 1)) ~ "|$n" } }
    is (await f(3)), 'b|1|2|3', 'recursive start/await, 1st call';
    is (await f(3)), 'b|1|2|3', 'recursive start/await, 2nd call';
    is (await f(2)), 'b|1|2',   'recursive start/await, 3rd call with a new depth';
}

# await OUTSIDE the start block: used to be wrong on the very FIRST call.
{
    sub k($n) { $n <= 0 ?? "b" !! (await start { k($n - 1) }) ~ "|$n" }
    is k(3), 'b|1|2|3', 'await start { ... } recursion, 1st call';
    is k(3), 'b|1|2|3', 'await start { ... } recursion, 2nd call';
}

# Routing through a `my` instead of a parameter used to be worse (b|0|0|0).
{
    sub g($m) { my $n = $m; start { $n <= 0 ?? "b" !! await(g($n - 1)) ~ "|$n" } }
    is (await g(3)), 'b|1|2|3', 'recursion through a `my` copy of the parameter';
}

# Two-branch recursion: used to hang deterministically.
{
    sub fib($n) { start { $n < 2 ?? $n !! await(fib($n - 2)) + await(fib($n - 1)) } }
    is (await fib(4)), 3,  'two-branch recursion through start (fib 4)';
    is (await fib(8)), 21, 'two-branch recursion through start (fib 8)';
}
