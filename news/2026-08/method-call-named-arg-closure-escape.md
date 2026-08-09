# Closure literal in a method-call named argument now marked escaping

A closure literal passed as a **named** method-call argument (`Store.new(now
=> { $x })`, or the colon-call form `Store.new: now => { $x }`) was not
recognized as escaping, so a captured-and-mutated lexical (`$x`) it names
was snapshotted by value instead of getting a shared `ContainerRef` cell.
A long-lived worker thread invoking the stored closure kept seeing the
value from spawn time, never the caller's later mutations — e.g. a fake
clock closure `now => { $fake-now }` used to simulate session expiry in
Cro's session middleware tests.

## Root cause

Escape analysis marks a closure-literal call argument escaping so
`box_captured_lexicals` gives its captured-and-mutated locals a shared
cell (see `src/compiler/helpers_call_args.rs`). The function-call compile
path (`expr_call.rs`) already unwrapped a fat-arrow named argument
(`key => value`) before testing `is_closure_literal_arg` on the value, but
the two method-call compile paths (`expr_method.rs`, one for the general
method-call form and one for the postfix/colon-call form) tested
`is_closure_literal_arg` on the raw, un-unwrapped argument. A fat-arrow
named argument is `Expr::Binary { op: FatArrow, .. }`, never itself a
closure-literal variant, so `now => { $x }` was always classified
non-escaping on the method-call paths — the sub-call path fixed by #5891
covered only half the surface.

## Fix

Extracted the fat-arrow unwrap into a shared
`unwrap_named_arg_value` helper next to `is_closure_literal_arg` in
`helpers_call_args.rs`, and used it at all three call sites: the existing
one in `expr_call.rs` (previously duplicated the unwrap inline) and the
two in `expr_method.rs` that were missing it.

## Verification

- The minimal repro (a `Store` with `has &.now`, worker thread reading it
  over a `Channel`, main thread mutating `$x` between reads) now matches
  raku exactly, in both the parenthesized and colon-call forms.
- The W5–W9 control variants from the original diagnosis (single-threaded,
  positional closure, sub-call, `&getter` variable, fresh `start`) are
  unchanged.
- New pin: `t/closure-named-arg-method-escape.t`.
- Full `make test` and the whitelisted `S17-supply/*.t` roast files were
  spot-checked with no regressions. Note: this fix addresses the specific
  escaping gap described here; the full Cro session-expiry integration
  test still has other, separately-tracked failures unrelated to this
  code path.
