# The ecosystem sweep was throwing away a distribution that passes

`DateTime::React` measured `green 1/1`, and the one file it counted was
`t/00-sanity.t` — three lines around a bare `ok True`. Its real suite,
`t/01-basic.t`, was stamped `flaky: true` and demoted to `no_baseline`, so the
distribution's only interesting measurement never reached the KPI.

It is not flaky. It passes 8/8 under mutsu, exactly as it does under rakudo, and
the two interpreters agree on every part of the module's API — the `term:<…>`
exports resolve bare and with parens, the `state`-cached `Supply` is the same
object on a second call, the compatibility `&minute-shifts` is a `Sub`, and
`Shift::Minute` composes `DateTimeEventish` and reports `type => 'minute'` with a
60-second `next-at - time`. Nothing in mutsu was broken here. The measurement was.

## The file always ends at the same second

`t/01-basic.t` waits for two minute rollovers:

```raku
my $now = DateTime.now;
my $s   = 60 + ceiling 60 - $now.second;
...
sleep $s + 1;
```

So its runtime is `121 - second-of-minute` — which means it *always finishes at
second-of-minute 1*, whatever second it started at. Measured on this box:

```
raku start second-of-minute=41.84   end=01.55   duration=79.72
raku start second-of-minute=01.56   end=01.69   duration=120.12
```

The sweep runs rakudo and then mutsu on the same file back to back, and retries a
non-passing file up to three times. Every one of those runs therefore *starts* at
second-of-minute ≈1 — the single phase at which the file needs ~120.1s, just over
the sweep's 120s default budget. The first attempt timed out, and so did the
retries, because a same-budget retry re-rolls a die that is not random: whatever
decides the file's runtime also decides when the previous attempt ended. The
120s budget was the whole failure, and it was invisible in the record, which said
only `flaky`.

## A timeout is a claim about the budget

`measure()` in `scripts/ecosystem-sweep.py` now treats the first timeout as
provisional. It buys one retry at twice the budget, and does not itself count
towards `flaky`: until a longer run has disproved it, a timeout says something
about the budget, not about the file. If the longer run passes, the file was
never non-deterministic, only slow. If it times out too, that verdict is believed
and the retries stop — so a genuine hang costs no more wall clock than the two
same-budget retries this replaces, and a green corpus costs exactly what it did
before.

The rule is pinned by `scripts/ecosystem-sweep.py --self-test`, wired into the
sweep workflow's existing self-test step alongside `ecosystem-ci` and
`ecosystem_common`: a slow file passes and is not flaky, a hang is still a
timeout and stops after one escalation, a `die` still retries at the same budget,
a later verdict replaces a budget-limited timeout, and `--attempts 1` escalates
nothing. `measured.harness` goes to `2`, so records say which method produced
them.

After the change, re-measured with `--only`:

```
[1/1] DateTime::React                          green        2/2 files
```

`t/01-basic.t` is `parity`, 8 assertions on each side, mutsu at 120.18s — the run
that used to be discarded. Baseline files for this distribution go 1 → 2 and
baseline assertions 1 → 9.

## What else this recovers

Across the 4907 file-entries currently in `ecosystem/`, 32 are timeouts and every
single one sits at 120.1s — nothing in the corpus times out because it hung *well*
past the budget; they all die within a tenth of a second of it. Some of those are
real hangs that will keep timing out at 240s and be recorded as such, which is the
point: the escalated retry is what tells the two populations apart, and the ledger
could not distinguish them before. `DateTime::React` was simply the clearest case,
because it is the only entry in the corpus that passed at 119.4s on one attempt
and timed out on another.
