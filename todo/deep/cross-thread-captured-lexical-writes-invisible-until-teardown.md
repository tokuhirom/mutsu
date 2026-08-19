# A captured lexical written from another thread stays invisible until scope teardown

A closure running on one thread can increment a captured outer lexical, and a closure
running on another thread reading that same lexical keeps seeing the pre-write value.
The writes are not lost -- they appear all at once once the enclosing scope tears down --
so the symptom is a *staleness* window, not a lost update.

## Repro

```raku
my $closed = 0;
my $sod = Supply.on-demand: -> $s { start { $s.emit(42); $s.done; } },
                            closing => { $closed++ };
my $ticks = 0;
react {
    whenever Supply.interval(0.05) {
        $ticks++;
        whenever $sod { };
        note "tick $ticks sees closed=$closed";
        done if $ticks >= 4;
    }
}
say "after react: closed=$closed";
```

mutsu:

```
tick 1 sees closed=0
tick 2 sees closed=0
tick 3 sees closed=0
tick 4 sees closed=0
after react: closed=3
```

raku:

```
tick 1 sees closed=0
tick 2 sees closed=1
tick 3 sees closed=2
tick 4 sees closed=3
after react: closed=4
```

The `closing` callback fires and `$closed` reaches 3 either way. The difference is that
mutsu's react thread cannot observe it while the react block is still running.

## It is specifically cross-thread, not "closures in react"

Change only the on-demand body so the `closing` callback runs on the react thread instead
of a `start` thread (`-> $s { $s.emit(1); $s.done; }`, no `start`), and mutsu is correct:

```
tick 1 sees closed=0
tick 2 sees closed=1
tick 3 sees closed=2
tick 4 sees closed=3
```

So the machinery for capturing and mutating `$closed` works; what fails is propagating a
write made on thread A to a reader on thread B before the scope that owns the variable
ends. This has the shape of the `env_dirty` dual store / per-thread env copy described in
CLAUDE.md's architecture section -- each thread appears to work against its own view of
the environment, reconciled only at teardown -- so it is high blast radius and needs a
design decision (which is why this is filed under `todo/deep/`, not as a ticket).

Compare `t/lock.t`'s history, where the same area produced a genuinely lost update
(#4167): the fix routed shared-array pushes through a dedicated atomic store. That was a
targeted patch for one construct; the general rule "a captured lexical is one cell shared
by every thread that captured it" is still not established.

## A live test is currently hiding this

`t/react-nested-whenever-on-demand-close.t` subtest 1 is exactly this shape:

```raku
react {
    whenever Supply.interval(0.02) {
        whenever $sod { }
        done if $closed;
    }
    whenever Promise.in(5) { done }
}
ok $closed, "async on-demand closing fires from a nested whenever (closed=$closed)";
```

`done if $closed` never fires because of the staleness, so the react runs until the 5s
backstop and the interval ticks 250 times (`closed=250`). The final `ok $closed` then
passes, because by then the scope has torn down and the writes are visible. The file
therefore reports green while costing 5 seconds of every `make test` -- it was the 4th
slowest of all 3261 files in a full timing sweep, and the whole 5.09 s is the backstop.
Its own comment says the backstop exists so that "a genuine regression (closing never
fires) fails cleanly with closed=0 rather than hanging"; in practice it is masking a
different genuine defect instead.

When this is fixed, subtest 1 should complete in ~0.04 s like the synchronous subtest 2
does, and the file is worth tightening to assert that (e.g. bound the observed tick count)
so the backstop cannot silently absorb a regression again.
