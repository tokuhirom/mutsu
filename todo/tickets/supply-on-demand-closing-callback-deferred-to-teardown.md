# `Supply.on-demand`'s `closing` callback fires late, batched at react teardown, not per-tap

Found while verifying the perf bonus claimed by
`todo/tickets/whenever-scope-discards-its-analysis-cc.md` (now fixed, see
`news/2026-08/whenever-scope-analysis-cc-boxing.md`). That ticket predicted
`t/react-nested-whenever-on-demand-close.t` subtest 1 would drop from ~5.1s to
~0.04s once the `analysis_cc_idx` boxing fix landed, because the file's
`done if $closed` backstop was assumed to be absorbing the same lost-write
defect. It does not: after the fix the file still takes ~5s and hits its
`Promise.in(5)` backstop, so a distinct, still-open bug remains.

## Repro

```raku
my $closed = 0;
my $sod = Supply.on-demand:
    -> $s { start { $s.emit(42); $s.done; } },
    closing => { $closed++; say "closing fired, closed now $closed"; };
my $ticks = 0;
react {
    whenever Supply.interval(0.02) {
        $ticks++;
        whenever $sod { }
        say "tick $ticks closed=$closed" if $ticks <= 5 || $closed;
        done if $closed;
    }
    whenever Promise.in(1) { say "backstop fired"; done }
}
say "final closed=$closed ticks=$ticks";
```

mutsu output (2026-08-20, after the `analysis_cc_idx` fix):

```
tick 1 closed=0
tick 2 closed=0
tick 3 closed=0
tick 4 closed=0
tick 5 closed=0
backstop fired
closing fired, closed now 1
closing fired, closed now 2
...
closing fired, closed now 24
```

Every single `closing fired` print happens AFTER `backstop fired` — i.e. after
the react block's `Promise.in(1)` backstop already tore the block down. The
`whenever Supply.interval(0.02) { ... }` body re-taps `$sod` (a fresh on-demand
subscription) every 0.02s, each of which should fire its own `start { emit;
done }` producer promptly and then invoke its `closing` callback once the tap
closes — but none of the ~50 closings (for a 1s run at 0.02s interval) happen
until teardown, where they suddenly all fire back-to-back.

This is NOT the lexical-cell-aliasing bug the other ticket fixed: the `closing`
callback DOES eventually run and DOES correctly mutate the real `$closed`
(confirmed by the final tally matching the tick count), so the cross-thread
cell sharing is sound. The defect is in *scheduling* — the on-demand Supply's
per-tap producer/closing lifecycle is not being driven promptly by the react
event loop; something is deferring ~50 pending on-demand subscriptions' worth
of completion signals until the loop actually exits.

## Affected files (starting points, not yet root-caused)

- `t/react-nested-whenever-on-demand-close.t` — the test absorbing this via a
  5s backstop (see its own comment, written when this was still attributed to
  the other ticket).
- Wherever `Supply.on-demand` producer/closing dispatch is implemented
  (`runtime/` — search for `on-demand` and `on_demand`).
- The react/whenever drive loop (`vm/vm_react_loop.rs`) — the mechanism that
  polls for completed promises/channel sends from taps; on-demand subscription
  completions may be queued somewhere this loop does not check per-iteration.

## Why this is a `todo/tickets/` item, not `todo/deep/`

The repro is small and deterministic (not flaky — reproduces every run), and
the observed behavior (correct final value, wrong timing) narrows the search
to a scheduling/polling gap rather than a data-aliasing problem. It likely does
not need an ADR, but root-causing it needs a session of its own rather than
riding along on the `analysis_cc_idx` PR.

## Suggested next step

Instrument (gdb breakpoint, not eprintln — see CLAUDE.md debugging guidelines)
the on-demand Supply's `closing` invocation call site to find what it is
waiting on, and compare against how a plain (non-on-demand) `Supply.interval`
whenever gets its per-tick callback dispatched promptly. Also check whether
`whenever Supply.interval(0.02) { whenever $sod { } }`'s INNER `whenever $sod {
}` registration itself is what's stalled (rather than the closing callback) —
i.e. whether the nested tap only gets processed once the outer react loop
finally drains a backlog at exit.
