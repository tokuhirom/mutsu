# `take` with no `gather` is an error, not a quiet return

```raku
sub mention-me() { take 1 }
mention-me;
```

mutsu exited 0 and printed nothing. rakudo dies with `take without gather` and
names the routine. The cause is the value the signal carries: `take` is raised
as a control signal whose `return_value` holds the taken value, and every
routine-call boundary treats an error carrying a `return_value` as an explicit
`return`. `CX::Warn` had already been carved out of that rule for the same
reason; `take`, `emit` and `done` needed the same carve-out, expressed once as
`RuntimeError::is_yield_signal` and applied at all seven boundaries — the plain,
typed and fast compiled-call paths, the named path, closures, and the two
method-dispatch exits. Doing only the first two was not enough and failed in a
way worth remembering: the *second* call to a routine is dispatched by
`call_compiled_function_fast`, so a repro that calls the routine once looked
fixed while the test file that called it three times did not. A `take` inside a `gather` is unaffected — it never
becomes an error at all, because both raise sites check the gather depth first.

Three smaller things travelled with it, all on the same escaping-signal path:

- The signal reported itself as `CX::Take` when nothing consumed it, even though
  it already carried a fully-formed `X::ControlFlow`. Its message is now the
  exception's (`take without gather`, `emit without supply or react`); the
  `control` flag, which is what routes it to a `CONTROL` block, is unchanged.
- `try { take 1 }` did not trap it. `RuntimeError::is_illegal_control` — the
  predicate that stops `try` passing a signal through when nothing upstream
  could consume it — knew about `next`/`last`/`redo` but not `take`. rakudo's
  `try` leaves `$!` holding `take without gather`, and now so does mutsu's.
- It printed with no backtrace. `exec_one` attaches one only to errors with no
  `control` flag; a signal nothing can consume is an error in all but that flag,
  so it gets one too. `roast/integration/error-reporting.t` asks for exactly
  this (`err => rx/'mention-me'/`).

Separately, an unterminated `#`{...}` comment reported "Opening bracket required
for #` comment" — the bracket was there, it just never closed. It now says
`Couldn't find terminator for #` comment (opened on line N)`. The wording avoids
"at line N" on purpose: `error_render::strip_internal_location` removes that
phrase as an internal detail.

Pin: `t/take-without-gather.t` (all ten assertions verified against `raku`). Its
backtrace assertion deliberately uses a routine it has not called before —
mutsu loses the `in sub` frame on a *repeated* call, for `die` as much as for
`take`, which is filed as
`todo/tickets/repeat-call-loses-backtrace-frame.md`.
