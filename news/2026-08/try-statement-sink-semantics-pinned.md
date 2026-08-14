# A `try` statement's sink placement is verified rakudo-conformant, and its motivating roast failure was already fixed

`roast/integration/advent2009-day20.t` used to abort after 11 of its 21
assertions under `MUTSU_REAL_TEST=1`: `Test.rakumod`'s `eval_exception`
helper —

```raku
sub eval_exception($code) {
    try { EVAL ($code); }
    $!
}
```

— let a `Stub code executed` exception escape the `try` entirely and kill the
file when called on `map -> $x, $y { ... }, 1..6`. The original hypothesis was
that mutsu sinks a `try` block's discarded value *outside* the try's own
protection, and that this sink point was wrong.

A deep-dive (2026-08-10) built a raku-vs-mutsu probe matrix across ~40
snippets and found the motivating symptom was **already fixed on `main`** by
PR #6115 (merged 2026-08-09): the `...` stub gained `fail()` semantics instead
of `die()` (`news/2026-08/stub-fail-semantics.md`), and unhandled Failures
throw in string-coerce context. `advent2009-day20.t` passes 21/21 today, both
plain and under `MUTSU_REAL_TEST=1`.

The matrix also answered the semantics question the ticket left open: **mutsu's
sink placement is correct, not a bug.** Rakudo's own rule, derived empirically:
a statement-position `try { ... }`'s value IS sunk (there is no "call results
are exempt" carve-out), and sink context propagates into an immediately-invoked
block's final statement — but a plain `try`'s handler wrapper interposes and
stops that propagation, so the tail value escapes un-sunk and is sunk by the
*enclosing* statement, outside the try's protection. Raku itself throws
uncaught in exactly those cases (verified: P4/P5/P12/P13/Q4-Q6/Q12 all escape
in real raku too, and an enclosing `CATCH` can catch the escape — Q9). Moving
mutsu's `SinkPop` inside the trap, which the original hypothesis suggested,
would have been a regression, not a fix — it would break the unit-scope-escape
parity #6115 established.

What remained genuinely divergent all traced to a *different*, already-tracked
mechanism (eager `Seq` reification at call/assignment boundaries, not
try/sink placement) — mutsu is uniformly more forgiving than raku there, never
less. Split out to `todo/deep/deferred-seq-materialization-destroys-the-original.md`'s
new "Residual try-cell divergences" section.

Pinned by the new `t/try-sink-semantics.t` (14 assertions, verified against
both `target/debug/mutsu` and real `raku`).
