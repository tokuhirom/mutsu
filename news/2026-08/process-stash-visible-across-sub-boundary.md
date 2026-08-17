# `PROCESS::<$X>` is now visible from a callee sub, not just the writing frame

`Log::Timeline`'s own upstream test suite (bundled as a `Cro::HTTP`
dependency) failed on its core logging tests:

```
$ mutsu -I modules/Log-Timeline/lib -I modules/CBOR-Simple/lib -I modules/TinyFloats/lib t/logging.rakutest
ok 1 - Logging an event is a no-op if no output
ok 2 - Can log an event with no data
ok 3 - Can log an event with data
not ok 4 - Got expected output
# expected: '2'
#      got: '0'
not ok 5 - First event logged correctly
# expected: {:data(${}), :event(Bool::True), :parent-id(0), :type(My::Test::EventA)}
#      got: Any
```

## Root cause

`Log::Timeline` records its output backend via `PROCESS::<$LOG-TIMELINE-OUTPUT>
= $output;` at the mainline, then reads it back from deep inside its logging
subs. Isolated repro:

```raku
PROCESS::<$FOO> = 42;
sub reader() { say PROCESS::<$FOO>; say PROCESS::<$FOO>.defined; }
reader();
# raku:  42 / True
# mutsu: Nil / False  (before this fix)
```

`package_stash_value`'s `PROCESS::` branch (`src/runtime/accessors_stash.rs`)
built its stash view by scanning only `self.env` — the CURRENT frame's own
dynamic-var store — instead of the whole dynamic-scope caller chain. A
plain `$*FOO` read already correctly walks that chain (verified: an
explicitly-declared `my $*FOO` is visible from a sub), but `PROCESS::<$X>`'s
ad-hoc reimplementation of the same lookup did not, so a value set at an
outer frame silently vanished once read from a callee.

## Fix

Reused `dynamic_pseudo_stash_entries` — the same caller-chain walk already
backing the `DYNAMIC::` pseudo-stash — instead of only scanning `self.env`,
remapping its `$*NAME`/`@*NAME`/`%*NAME`-spelled keys to `PROCESS::`'s
twigil-less `$NAME`/`@NAME`/`%NAME` stash keys.

With this fix, `Log::Timeline`'s `t/logging.rakutest` tests 1-6 now pass,
matching raku exactly (previously only tests 1-3 passed). The file's second
`given` block (`.task`/`.start`/`.end`, testing 7 onward) still diverges —
filed separately as
`todo/tickets/log-timeline-task-leave-phaser-process-reset.md`, since it is
a distinct issue not yet root-caused (a bare `LEAVE PROCESS::<...> = Nil`
was checked in isolation against raku and already works correctly in mutsu,
ruling out the most obvious suspect).

New test: `t/process-stash-visible-across-sub-boundary.t`.
