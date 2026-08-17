# `Log::Timeline`'s `.task`/`start`/`end` block still fails after the PROCESS::/dynamic-var cross-frame fix

Found while fixing `log-timeline-cbor-output-format-mismatch.md` (now
`news/2026-08/process-stash-visible-across-sub-boundary.md`). That fix
resolved the ticket's own reported symptom (`t/logging.rakutest` tests 1-6
now pass, matching raku), but running the full file further shows a SECOND,
separate divergence from test 7 onward.

## Divergence

```sh
git clone https://github.com/raku-community-modules/Log-Timeline.git /tmp/log-timeline
cd /tmp/log-timeline
timeout 20 mutsu -I /path/to/mutsu/modules/Log-Timeline/lib -I /path/to/mutsu/modules/CBOR-Simple/lib -I /path/to/mutsu/modules/TinyFloats/lib t/logging.rakutest
```

Test 7 ("Logging a task is a no-op if no output (using .task)") now fails,
and the whole SECOND `given FakeOutput.new -> $output {...}` block
(`t/logging.rakutest` lines 55-134, covering `.task`/`.start`/`.end`)
produces the same "expected N, got 0" / "expected {...}, got Any" pattern
the FIRST block used to show before the PROCESS:: fix.

## Suspected root cause (not diagnosed)

The first block (lines 29-45) does:
```raku
given FakeOutput.new -> FakeOutput $output {
    PROCESS::<$LOG-TIMELINE-OUTPUT> = $output;
    LEAVE PROCESS::<$LOG-TIMELINE-OUTPUT> = Nil;
    ...
}
```
and test 7 (right after that block closes) checks that logging is a no-op
again -- i.e. it depends on the `LEAVE` phaser's `PROCESS::<...> = Nil`
actually firing and being visible to the code that runs after the `given`
block exits. Since the fix made `PROCESS::` reads correctly walk the whole
caller-chain (`dynamic_pseudo_stash_entries`), a `LEAVE`-phaser write not
correctly re-registering in that chain (or the second `given` block's own
`PROCESS::<$LOG-TIMELINE-OUTPUT> = $output` write landing in a frame the
chain does not see with the right precedence) is the likely culprit, but
this is unverified -- worth a reduced repro isolating `LEAVE` + `PROCESS::`
interaction from Log::Timeline's own Task/Event mechanics before touching
code.

## Reproduce (isolated LEAVE+PROCESS:: check -- RULED OUT, checked against raku)

```raku
sub scope() {
    PROCESS::<$X> = 1;
    LEAVE PROCESS::<$X> = Nil;
    say "inside: ", PROCESS::<$X>;
}
scope();
say "outside: ", PROCESS::<$X>;
```
raku: `inside: 1` / `outside: (Any)`. mutsu: `inside: 1` / `outside: Nil`
(same value, cosmetic `Nil` vs `(Any)` gist difference only -- NOT a bug).
So plain `LEAVE PROCESS::<...> = Nil` in isolation already works correctly
in mutsu; the suspected root cause above is WRONG. The real trigger is
something specific to the `given FakeOutput.new -> $output {...}` shape,
or to `Log::Timeline::Task`'s own mechanics (`.log`/`.start`/`.end`), not a
bare LEAVE+PROCESS:: interaction. Next step: reduce further by replacing
`My::Test::TaskA.log: {...}`/`.start`/`.end` calls with something that
exercises the SAME PROCESS::<$LOG-TIMELINE-OUTPUT> read pattern
`Log::Timeline::Task`'s methods use, without the full module, to see if a
second `given` block re-assigning the same PROCESS:: var is itself the
trigger (e.g. two sequential `given FakeOutput.new -> $output { PROCESS::<$X>
= $output; ...; }` blocks, second one's writes/reads checked for
correctness).

## Scope note

Out of scope for the PROCESS::-cross-frame fix (a distinct, narrower bug):
that fix is a genuine, general improvement (verified against raku) and
should not be blocked on this residual issue. This ticket also does not
address `Log::Timeline`'s CBOR/JSON-lines/socket OUTPUT SERIALIZATION
format itself (the original ticket's title) -- `output-cbor-sequence.rakutest`,
`output-json-lines.rakutest`, `output-socket.rakutest` were never reached in
this investigation and may have their own separate gaps once `logging.rakutest`
is fully green.
