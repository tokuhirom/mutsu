# Repeated big-Int addition (growing-magnitude Fibonacci-style loop) is roughly 14x slower than raku

Discovered via the doc-diff harness on `raku-doc/doc/Language/faq.rakudoc` (around line 1108);
the harness bucketed it as a `mutsu-error` because the 100,000-iteration example (with a
multi-thousand-digit final result) exceeds the harness's per-example timeout, but this is a
**performance gap, not a correctness bug** — smaller iteration counts produce the correct
result, just slowly.

## Repro

```raku
my ($prev, $current) = (1, 0);
for (0..10_000) {
    ($prev, $current) = ($current, $prev + $current);
}
print "done\n";
```

Timing (release... actually debug `target/debug/mutsu` build used for this measurement, see
note below):
- `raku`: ~0.31s
- `mutsu` (`target/debug/mutsu`): ~4.4s (roughly 14x slower)

The doc's full example runs the loop 100,000 times (final `$current` has ~20,000 decimal
digits); at that size mutsu times out under the harness's budget while raku completes and prints
the correct multi-thousand-digit number (verified the *correctness* of mutsu's output at smaller
iteration counts — e.g. 1,000 iterations — matches raku's digit-for-digit).

Note: this timing used the **debug** build per the correctness-check convention in this repo,
but since the loop count scales roughly quadratically (digit count grows ~linearly with `n`, so
per-addition cost also grows ~linearly, giving ~O(n^2) total), the gap is large enough that it is
very likely still significant on a release build — worth re-measuring with
`target/release/mutsu` before prioritizing, per the "release is for wall-clock only" convention.

## Root cause hypothesis (unconfirmed)

Likely inefficiency in the big-integer (`BigInt`) addition path in `src/builtins/arith.rs` (or
wherever bignum `+` is implemented) — e.g. avoidable allocation/reparsing/string-roundtripping
per addition, or not using the underlying bignum library's in-place/fast-path addition
efficiently. This is the same general "measured perf gap" shape as the existing
`uniname-sort-performance-gap.md` ticket (a specific slow builtin operation, not a correctness
bug), not something to guess at without profiling (`perf`/flamegraph) first.

## Affected files (starting point)

- `src/builtins/arith.rs` — bignum `+` implementation
- `src/value.rs` — `Value`'s bigint variant representation, to check for unnecessary
  clone/realloc on each addition
