# How to survey a battery slot

The criteria for choosing a battery are in
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria). This document is the
**procedure** that turns those criteria into numbers, so every future slot is
decided the same way and the evidence is reproducible.

It exists because the first two slots were nearly decided on stale prose.
`PLAN.md` carried "Template::Mustache, 91/92 specs" as its readiness note; when
that was actually re-measured on 2026-07-25 the engine passed **1 of its 13**
upstream test files. **Never trust a readiness claim you did not just measure.**

## 0. Rules that override everything

- **License is a hard gate.** No declared license anywhere → the candidate is
  out, however good it is. (`HTML::Template` and `Text::Template` were dropped
  from the template slot on exactly this.) We already ship one provisional
  exception, `Encode`, and do not want a second.
- **Measure `raku` first, then mutsu.** A module that fails under mutsu *and*
  under raku is dead upstream, not a mutsu bug. Skipping this step nearly
  produced a bogus bug report against `Template::Mojo` — its `v0.1` (2017) is
  broken everywhere, while the current `0.2.2` is 5/5 under raku and 0/5 under
  mutsu, which is a real and very different finding.
- **A candidate that ships no tests is structurally disqualified**, or close to
  it: `scripts/battery-testsuite.sh` is the entire verification story for a
  bundled library, and there is nothing to gate.

## 1. Enumerate the field from the ecosystem index, not from memory

The local Zef mirrors are the authoritative list and are already on disk:

```
~/.zef/store/rea/rea.json     # REA archive — has release-date, source-url
~/.zef/store/fez/fez.json     # fez — usually the newest versions
```

Together they carry ~2500 distinct distribution names. Filter on name,
`description` and `tags` with a keyword regex for the slot. Refresh them with
`mzef update` if they look old.

`rea.json` is the more useful of the two: it carries **`release-date`**, which is
the maintenance signal, and `source-url`, which points at a tarball you can fetch
directly (no `git clone` guessing at repository names).

## 2. Compute the metrics

For each candidate collect, in this order:

| Metric | Where from | Why |
| --- | --- | --- |
| License | `META6.json` `license`, cross-checked against a shipped `LICENSE`/`LICENCE` and the README | hard gate |
| Runtime deps | `depends` (note: a `{test => …}` structure means **zero** runtime deps) | §2's second criterion; a 0-dep dist is dramatically cheaper to bundle |
| Version + release date | `rea.json` `version` / `release-date` | is it maintained, or abandoned in 2018? |
| **Dependents** | count dists in the index whose `depends` names it | ecosystem standing — far better evidence than stars or opinion |
| raku result | run its own suite under `raku` | the baseline |
| **mutsu result** | run its own suite under mutsu | the actual decision input |

The dependents count is the one people skip and it is often decisive: for the
template slot it separated `Template::Mustache` (11 dependents, incl. `Bailador`,
`Documentable`, `Pod::To::HTML`) from candidates with 0.

## 3. Run both suites

`tmp/tmpl-survey.sh` is the worked example: it reads a `name|url` list, fetches
each REA tarball, extracts it, and runs every `t/*.t` / `t/*.rakutest` with
`-I lib` (plus `-I t/lib` when present) from the dist's own directory. Swap
`MUTSU_BIN=raku` for the baseline pass.

Count a file as passing only when it emits a TAP plan and every planned test is
`ok` with no `not ok` — the same rule `scripts/battery-testsuite.sh` uses, so the
survey number and the future gate number mean the same thing.

**Run each test from the dist's own directory.** These suites reach for fixtures
by relative path; running them from elsewhere makes files die before their first
test and be miscounted as library failures.

### Reading the failures

The harness quotes the first non-TAP line as a hint. It is a **pointer, not a
diagnosis** — `Use of Nil in string context` is a *warning* in both
implementations and never fatal on its own. Treat that column as "where to start
reducing", and reduce against raku before concluding anything.

## 4. Write the record

Two files come out of a survey:

- `docs/batteries/<slot>.md` — the field, the metrics table, the rejections
  *with reasons*, and the leaning. Rejected alternatives matter as much as the
  winner (BATTERIES.md §2); a future maintainer must not have to re-survey to
  learn why something was passed over.
- `todo/` entries for every mutsu bug the survey exposed, with the minimal repro
  and the raku-vs-mutsu output. A survey that finds interpreter bugs and does not
  record them has thrown away most of its value.

The metrics table goes stale the moment a fix lands, so re-run the survey before
acting on it.

## 5. Expect the answer to be "fix mutsu first"

For the template slot, *every* credible candidate was healthy under raku and
broken under mutsu, so the survey's real output was a work list, not a winner.
That is a normal outcome at this stage of the project and is exactly why the
raku baseline column is mandatory: it converts "this module doesn't work" into
"this is a mutsu bug worth N test files", which is schedulable.

## Worked examples

- [templates.md](templates.md) — the full field for the template slot.
- [http-client.md](http-client.md) — the shape of the record *after* a decision.
- [http-deps.md](http-deps.md) — a multi-module layer bundled as one slot.
