# `Log::Timeline::Task.log`/`.start`/`.end` do not see a `PROCESS::` output var set in the calling `given` block

Split off from `news/2026-08/leave-phaser-if-given-not-firing.md` after that
fix (the `LEAVE`-phaser-never-firing-in-`if`/`given` bug) measurably advanced
`Log::Timeline`'s `t/logging.rakutest` (tests 1-9 now pass, up from 1-6) but
tests 10-30 still fail — a deeper, separate gap.

## Repro

```sh
timeout 30 target/debug/mutsu -I modules/Log-Timeline/lib -I modules/CBOR-Simple/lib -I modules/TinyFloats/lib \
    path/to/Log-Timeline/t/logging.rakutest
```

Minimal shape (from the dist's own test, lines 54-67):

```raku
class FakeOutput { has @.entries; method record($e) { @.entries.push($e) } }
class My::Test::TaskA does Log::Timeline::Task['TestApp', 'Test Cat 1', 'Task A'] { }

given FakeOutput.new -> FakeOutput $output {
    PROCESS::<$LOG-TIMELINE-OUTPUT> = $output;
    LEAVE PROCESS::<$LOG-TIMELINE-OUTPUT> = Nil;

    my $run = False;
    My::Test::TaskA.log: { $run = True; }
    say $run;               # True in both — the block callback DOES run
    say $output.entries.elems;   # raku: 2   mutsu: 0
}
```

`Log::Timeline::Task.log` internally reads `PROCESS::<$LOG-TIMELINE-OUTPUT>`
to decide whether/where to record start/end entries; when no output is set,
it's a documented no-op (tests 7-8, "Logging a task is a no-op if no output",
correctly pass both before and after this ticket's parent fix). Here, the
`given` block DID set the output, but the read from inside
`My::Test::TaskA.log`'s own method body — several call frames deeper,
through a role-composed method (`Log::Timeline::Task` is a parametric role)
— does not see it, behaving as if it were still unset.

## Relationship to prior `PROCESS::` fixes

This is a different direction from both prior fixes in this family:

- `news/2026-08/process-stash-visible-across-sub-boundary.md` fixed a READ
  not seeing a WRITE made in an ENCLOSING frame (a `PROCESS::` write, then a
  plain sub call reading it).
- `news/2026-08/leave-phaser-if-given-not-firing.md` fixed a `LEAVE`
  phaser not firing at all inside `if`/`given`, so its own `PROCESS::` write
  (the RESET back to `Nil`) never happened.
- **This ticket**: the INITIAL write (`PROCESS::<$LOG-TIMELINE-OUTPUT> =
  $output`, made directly in a `given` block's own body, not inside a
  further-nested `LEAVE`) does not appear to reach a method call several
  frames deeper — specifically through a PARAMETRIC ROLE's composed method
  (`Log::Timeline::Task[...]`'s `.log`). Not yet isolated to confirm whether
  the `given` block specifically is the trigger, or whether it's the
  role-composed-method-call boundary, or some interaction between the two.

## Suggested next step

Reduce further: try the same write-then-nested-method-read shape WITHOUT
`given` (a plain lexical scope + a role method call) and WITHOUT a role (a
plain class method) to bisect which boundary loses the write — `given`'s own
scope, or crossing into a role-composed method body. `Log::Timeline`'s own
`.log`/`.start`/`.end` implementation (`modules/Log-Timeline/lib/Log/Timeline.rakumod`)
is the ground truth for the exact read pattern to reduce.

## Severity

Moderate: blocks `Log::Timeline` (a real, useful bundled battery) from
recording anything at all in its test suite's primary usage shape, though
the module's own no-op-when-unset fallback means it fails silently (no
crash) rather than visibly breaking user programs. 21/30 of
`t/logging.rakutest` still red.
