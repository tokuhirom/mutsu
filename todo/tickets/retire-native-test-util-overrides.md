# Retire the native `Test::Util` / `Test::Tap` overrides — two bugs stand in the way

`exec_call` now lets an *imported* routine beat mutsu's native Test provider,
but only for the `Test` module's own export list
(`runtime::TEST_MODULE_EXPORTS`). The rest of `Interpreter::is_test_function_name`
— roast's `Test::Util` and `Test::Tap` helpers (`is_run`, `doesn't-hang`,
`is-path`, `is-eqv`, `is-deeply-junction`, `make-temp-file`, `make-temp-dir`,
`make-temp-path`, `group-of`, `doesn't-warn`, `warns-like`, `test-iter-opt`, …)
— still dispatches to the native implementation *even though the module is
really loaded from source* (`roast/packages/Test-Helpers/lib/Test/Util.rakumod`).

That override is a live rung-3 provider over a module mutsu can already parse
and load, so it should go the same way `Pod::To::Text` did. It was measured on
2026-08-01 by widening the guard to every `is_test_function_name` name and
running all 228 whitelisted roast files that `use Test::Util`. Exactly **two**
files broke, each for one general interpreter bug:

## 1. `IO::Path ~~ IO::Path` is always False (`roast/S32-io/io-path.t`)

Test::Util's `is-path` is `cmp-ok $got.resolve, '~~', $exp.resolve, $desc`.

```raku
my $a = IO::Path::Unix.new('/foo/').add('bar');
my $b = IO::Path::Unix.new('/foo/bar');
say $a.resolve.raku eq $b.resolve.raku;   # True in both
say $a.resolve ~~ $b.resolve;             # raku: True   mutsu: False
```

Rakudo's `IO::Path.ACCEPTS(IO::Path:D)` compares `.absolute`; mutsu falls
through to a generic instance smartmatch. This one is small and independent of
everything else here.

## 2. A `Proc::Async` output tap is only drained by `await`-ing *that* promise (`roast/S17-supply/interval.t`)

Test::Util's `doesn't-hang` accumulates the child's output in a `.stdout.tap`
closure and then does `await Promise.anyof: Promise.in($wait), $prog.start`. On
mutsu the tap never fires, so the `:out(/'Did not hang'/)` check sees `''`:

```raku
my $p = Proc::Async.new: $*EXECUTABLE.absolute, '-e', 'say "B"';
my $s = '';
$p.stdout.tap: -> $a { $s ~= $a };
my $pr = $p.start;
await Promise.anyof: Promise.in(10), $pr;
say $s.raku;        # raku: "B\n"   mutsu: ""
sleep 1; say $s.raku;   # mutsu: still "" — it is not a race
await $pr; say $s.raku; # mutsu: "B\n" — the tap fires *here*
```

This is by construction: `native_supply_mut_methods.rs` marks a Proc::Async
output supply `proc_output` and deliberately does **not** start a live-channel
consumer for it, because delivery happens once, later, in `replay_proc_taps` —
which is called only from `await`/`.result` when the awaited promise's own
result is a `Proc` (`builtins_system_async.rs`, `methods_promise.rs`). A
composite (`Promise.anyof`/`allof`) has no `Proc` result, so nothing replays.

The shallow fix is to teach the composite to replay its components' proc taps.
The real fix is that a tap should be *push*-delivered as the reader thread
produces output, the way ADR-0008 made the other supplies push-based (#4636) —
the "replay at await" design is what makes `.tap` observably different from
`react`/`whenever` on the same stream. Beware `S17-procasync/basic.t` test 37,
which the current comment cites as the reason delivery happens exactly once.

## Order

Fix 1 first (self-contained), then 2 (needs a design call, possibly an ADR
amendment to 0008), then widen the guard in `runtime/calls.rs` from
`is_test_module_export` to `is_test_function_name` and delete the native
overrides that are then dead. `t/test-fn-import-shadow.t` is the pin to extend.
