# Repro: recursion through a `start` block silently returns the wrong answer.
# See docs/worker-pool-probes.md and PLAN.md §6.
# Root cause: clone_for_thread seeds `shared_vars`, a flat NAME-keyed map
# (src/runtime/runtime_thread.rs:18-53), which cannot represent two
# concurrently-live bindings of the same name -- i.e. a recursive frame chain.
#
# raku prints, in order: b|1|2|3 / b|1|2|3 / b|1|2 / b|1|2|3 / b|1|2|3 / 3 / 21
# mutsu prints:          b|1|2|3 / b|3|3|3 / b|2|2 / b|1|1|1 / b|1|1|1 / <hangs>

# (1) await INSIDE the start block: first call correct, later calls corrupt.
sub f($n) { start { $n <= 0 ?? "b" !! await(f($n - 1)) ~ "|$n" } }
say "f(3) 1st: ", await f(3);   # raku b|1|2|3 | mutsu b|1|2|3
say "f(3) 2nd: ", await f(3);   # raku b|1|2|3 | mutsu b|3|3|3   WRONG
say "f(2) 3rd: ", await f(2);   # raku b|1|2   | mutsu b|2|2     WRONG

# (2) await OUTSIDE the start block: corrupt on the very first call.
sub k($n) { $n <= 0 ?? "b" !! (await start { k($n - 1) }) ~ "|$n" }
say "k(3) 1st: ", k(3);         # raku b|1|2|3 | mutsu b|1|1|1   WRONG
say "k(3) 2nd: ", k(3);         # raku b|1|2|3 | mutsu b|1|1|1   WRONG

# (3) Two-branch recursion: hangs deterministically on mutsu (exit 124).
#     This is the symptom PLAN §6 used to record as the whole bug.
sub fib($n) { start { $n < 2 ?? $n !! await(fib($n - 2)) + await(fib($n - 1)) } }
say "fib(4): ", await fib(4);   # raku 3
say "fib(8): ", await fib(8);   # raku 21
say "ALL DONE";
