# A Failure produced by sink-forcing a lazy Seq escapes `try` and aborts the enclosing routine

When the last statement of a `try { ... }` block is a call whose result is a
lazy `Seq`, mutsu sink-forces that Seq, and if forcing it produces a `Failure`
the Failure **escapes the `try`**: every remaining statement of the enclosing
routine is skipped and the Failure becomes that routine's return value.

## Repro (no Test module)

```raku
sub myliveok(Callable $code, $reason = '') {
    try {
        $code();
    }
    say "  after try; err defined = ", $!.defined;
    return 'reached-the-end';
}
say "result -> ", myliveok({ my $s = map -> $x, $y { ... }, 1..6; }, 'x').raku;
say "still running";
```

```
raku                                     mutsu
  after try; err defined = False         (line never printed)
result -> "reached-the-end"              result -> Failure.new("Stub code executed")
still running                            still running
```

Two divergences are stacked here, and the second is the important one:

1. raku never even throws — `$!.defined` is `False`, so sinking that `map` Seq
   did not run the stub callback at all. mutsu forces it.
2. Once mutsu has a `Failure`, `try` does not trap it. It propagates out of the
   `try` block, out of the rest of the routine body, and lands as the routine's
   return value — silently, with nothing printed.

(2) is what makes this dangerous: a routine can return a `Failure` from the
middle of its body with no diagnostic at all, which is exactly the "runs N of M
tests and prints no error" signature that
`t/signature-introspection-gaps.t` shows under `MUTSU_REAL_TEST=1`
(`# You planned 8 tests, but ran 7`, exit 255, no error line). The real
`Test.rakumod`'s `lives-ok` is `try { $code(); }` followed by `proclaim(...)`,
so `proclaim` is simply never reached.

## Narrowing already done

The escape needs the **sink** — `try { my $s = $code(); }` (assigning the result
inside the try, so nothing is sunk) behaves correctly and reaches the rest of
the routine. So does `try { fail "boom" }`, `try { die "boom" }`,
`try { $code() }` where `$code` is `{ fail ... }` or `{ die ... }`, and
`try { my $s = map -> $x { ... }, 1..2; }` written inline. Only the shape
"sink-force a lazy Seq whose forcing raises" escapes.

## Where to look

The sink of a `try` block's final value, and whatever path forces a `Seq` in
sink context — the raise from inside that force is not being routed to the
`try` handler that is lexically enclosing it. See `vm_control_ops.rs`'s try/catch
handling and the `sink_seq_body` / `reify_or_consume_seq_target(target, "sink")`
path in `vm_helpers_lazy.rs`.

Divergence (1) — that raku does not force the `map` at all here — should be
settled at the same time, since fixing only (2) would turn a silent abort into
a spurious caught exception (`lives-ok` would report a failure where raku
reports a pass).
