# `Promise.vow` protection is enforced

```raku
my $promise = Promise.new;
my $vow = $promise.vow;
$promise.keep;
CATCH { default { say .^name, ': ', .Str } };
```

- rakudo: `X::Promise::Vowed: Access denied to keep/break this Promise; already vowed`
- mutsu (before): no output at all — `$promise.keep` silently succeeded

mutsu's `Promise` had no notion of a vow being *taken*. `.vow` handed back a
`Promise::Vow` object without recording anything, and `.keep`/`.break` reported
`X::Promise::Vowed` purely as a mislabelled "already resolved" error.

## The protocol, established against rakudo 2026.06

Probed case by case with a **fresh promise per case** — the flag is consumed by
the very first attempt, so a shared promise pollutes every later probe and
produces a self-contradictory picture (the first pass at this genuinely did:
`Promise.kept(3).keep` looked like `Resolved` while `Promise.kept(3).break`
looked like `Vowed`, purely because the preceding line had consumed the vow).

A promise has exactly **one** vow, and `.vow`, `.keep` and `.break` all consume
it:

```
vow():    throw X::Promise::Vowed if vow_taken; vow_taken = 1; return Vow
keep(v):  throw X::Promise::Vowed if vow_taken; vow_taken = 1; !keep(v)
break(v): throw X::Promise::Vowed if vow_taken; vow_taken = 1; !break(v)
!keep(v): throw X::Promise::Resolved if status != Planned
Vow.keep(v) -> promise!keep(v)      # the vow holder never checks the flag
```

Consequences worth spelling out, all verified:

- A plain second `.keep` is `X::Promise::Vowed`, **not** `X::Promise::Resolved`
  — the first `.keep` took the vow.
- `Promise.kept` / `Promise.broken` hand back a settled promise whose vow was
  never taken, so `.keep` on one *is* `X::Promise::Resolved`, and `.vow` on one
  still succeeds.
- `Promise.start`, `.in`, `.at`, `.allof`, `.anyof` and `.then` are internally
  vowed: the runtime owns their resolution, so a user `.keep` on any of them is
  `X::Promise::Vowed`.

## The fix

`PromiseState` gained a `vow_taken` flag with two accessors: `take_vow()`
(consume, `false` if already taken) and `mark_vowed()` (the runtime claims it).
Only the *user-facing* `.vow`/`.keep`/`.break` methods consult the flag —
`keep`/`break_with`/`try_keep`/`try_break` deliberately do not, because they
stand in for the `Vow` object mutsu itself holds, which is what keeps every
internal resolution path (supply drives, `Proc::Async`, combinator waiters)
working unchanged.

`mark_vowed()` is called at exactly the constructors rakudo vows internally, and
pointedly **not** at `Promise.kept`/`Promise.broken`.

One knock-on: `Promise.in`/`.at` on a user `$*SCHEDULER` cue a synthesized
keeper block, which used to call `$promise.keep(True)` — now denied, since those
promises are vowed. The block resolves through a `Promise::Vow` instead, which
is how rakudo's own cued closure does it (`t/promise-in-honors-scheduler.t`
caught this).

The three exception messages moved out of per-throw-site `message` attributes
and into `format_exception_message()`, following the precedent set by the
`X::Proc::Async::*` family: a stored `message` attribute *shadows* that table,
so a hand-built `X::Promise::Vowed.new` would otherwise render differently from
a thrown one. `X::Promise::Resolved` and `X::Promise::CauseOnlyValidOnBroken`
derive their status from the exception's `promise` attribute the way rakudo
does. This also corrected `.cause`'s wording to rakudo's ("Can only call cause
on a broken promise", not "Can only call '.cause' on ..."), and let the
duplicated `X::Promise::Resolved` builder in `native_methods/concurrency.rs`
collapse onto the shared one.

## Coverage

`t/promise-keep-break-semantics.t` pins the whole protocol, including the
`Promise.kept` asymmetry and the internally-vowed `Promise.start`. The
documented repro used `Promise.in(10)` to hold the promise pending; the test
does **not** — holding the vow is itself what keeps it pending, and the test
resolves it through the vow at the end, so nothing sleeps and every wait is
bounded. `roast/S17-promise/basic.t`, which already asserted
`throws-like { $p.keep('eating') }, X::Promise::Vowed`, still passes, now for
the right reason.
