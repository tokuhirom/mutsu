# An `EVAL` that assigns to an outer lexical loses the write when the EVAL runs inside a closure or a routine

`EVAL '$a = 32'` writes through to the caller's `$a` at mainline and inside a
bare block, but the write is silently lost as soon as the `EVAL` runs inside a
closure that is *invoked* (`$c()`) or inside a named `sub`. No error is raised —
`$a` simply stays `Any`.

## Repro (measured 2026-08-28, release build, against `raku` as the oracle)

```
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; my $a; EVAL q|$a = 32|; say $a.raku'
32                                    # raku: 32   OK
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; my $a; { EVAL q|$a = 32| }; say $a.raku'
32                                    # raku: 32   OK
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; my $a; my $c = { EVAL q|$a = 32| }; $c(); say $a.raku'
Any                                   # raku: 32   WRONG
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; my $a; sub w(&c) { &c() }; w({ EVAL q|$a = 32| }); say $a.raku'
Any                                   # raku: 32   WRONG
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; my $a; sub w() { EVAL q|$a = 32| }; w(); say $a.raku'
Any                                   # raku: 32   WRONG
```

The construct is Test-free; the `Test` module is only the shape that surfaced it.

## Where it bites

`roast/S02-lexical-conventions/comments.t` test 41 under `MUTSU_REAL_TEST=1`:

```raku
my $a;
lives-ok { EVAL '$a = q{ 32 }' }, 'sanity check';
is $a, ' 32 ', 'sanity check';        # <- fails: got (Any)
```

The real `Test.rakumod`'s `lives-ok` is Raku-level and calls the Callable, so the
`EVAL` runs one closure-invocation deep and the write is lost. mutsu's native
`lives-ok` does not take that path, which is why the file passes natively and
regresses under the real module (`todo/deep/vendor-real-test-module.md`).

## Likely root cause (not verified)

`eval_eval_string` snapshots and restores env around the snippet
(`env_snapshot`, `eval_pre_lexicals`, `eval_shadowed` in
`src/runtime/system.rs` / `src/runtime/system_eval_string.rs`). It deliberately
keeps *assignments* to pre-existing keys while dropping the snippet's own `my`
declarations. That works while the caller's `$a` is a plain env key. Inside an
invoked closure / a compiled routine the caller's `$a` is reached through the
capture/locals machinery instead, so the write lands in the EVAL's borrowed env
and is discarded on the way out. Confirm with `rust-gdb` on the writeback path
rather than assuming.

## Why it is not a one-liner

It sits on the `locals`↔`env` dual store and the closure-capture writeback
(`capture_closure_env`, `apply_pending_rw_writeback`), which is the area
`PLAN.md` §6 / the Slice F campaign owns. A naive "always write env through"
would resurrect the leaks the snapshot/restore exists to prevent (a `my` inside
the EVAL must stay EVAL-scoped, `EVAL 'my $a = 999'` must not clobber the
caller's `$a`) — both are pinned by existing tests.
