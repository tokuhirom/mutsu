# doc-diff sweep of 2026-09-27: 117 issues from one pass over raku-doc

The doc-diff campaign ran again after an 18-day gap. The harness runs every
runnable raku-doc example under reference `raku` and under `mutsu`, and reports
where the two disagree. It covered all 443 files under `Type/` and `Language/`.
This time the sweep used a **release** `mutsu`, and the whole corpus finished in
about 20 minutes at `-j4` on a 4-core container. The 2026-09-09b debug-build
sweep took about 2.5 hours.

## What moved since 2026-09-09b

| | match | mismatch | crash | error-parity findings | of which mutsu-accepts |
|---|---:|---:|---:|---:|---:|
| 2026-09-09b | 2416 | 132 | 22 | 294 | 70 |
| 2026-09-27 | 2430 | 120 | 16 | 252 | 46 |

Every issue filed from the previous two sweeps (#7746–#7759, #7770–#7780) is
closed. The biggest drop is in `mutsu-accepts`, which went from 70 to 46: these
are programs raku refuses but mutsu ran to a clean exit. Several of them now fail
correctly, and differ only in the wording of the message.

## Triage

All of the sweep's ~390 findings were split across four read-only agents. Each
finding was re-run against `raku` v2026.07, reduced to a minimal repro, and
clustered by root cause. Findings were dismissed as noise when they came down to
hash ordering, `$*DISTRO`, missing files, or a Rakudo quirk where mutsu follows
the doc. The rest became **117 issues, #9766–#9882**. Each one has a repro, the
output of both implementations, and the doc lines it came from.

The highest-signal ones are the silent-acceptance and wrong-result bugs:

- The last statement of a program is never sunk, so an unhandled `Failure` there
  exits 0 (#9766).
- An undeclared bareword evaluates to its own name as a `Str` (#9768).
- `=:=` compares values instead of containers (#9769).
- A variable is visible in its own initializer (#9770).
- Type objects index arrays and feed `substr` as if they were the string `"(Any)"`
  (#9772, #9773).
- `no strict` variables are `Nil` instead of `Any` (#9775).

The table in `docs/doc-diff-backlog.md` maps every doc line to its issue, and
the committed raw data under `docs/doc-diff-sweep/` is refreshed to this sweep.
