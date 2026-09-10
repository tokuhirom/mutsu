---
name: ecosystem-sweep
description: Run the ecosystem parity sweep and turn its results into filed issues — shard-by-shard measurement that survives an ephemeral container, aggregating root causes without miscounting them, and reducing a cluster to a minimal repro. Use when running scripts/ecosystem-sweep.py, reading ecosystem/dists records, or triaging what the sweep found.
metadata:
  short-description: Measure the ecosystem and file what it finds
---

# Ecosystem parity sweep

Two jobs, and the second is the point: **run** the corpus sweep, and **turn what it finds into
filed issues**. A sweep whose findings are not filed has produced nothing — the container is
ephemeral and the records alone do not tell the next session what to fix.

- Decisions: [ADR-0085](../../../docs/adr/0085-ecosystem-testsuite-parity-measurement.md)
- Method, schema, CLI, runbook: [docs/ecosystem-parity.md](../../../docs/ecosystem-parity.md)
- Tracking issue: [#7785](https://github.com/tokuhirom/mutsu/issues/7785)

## 1. Running it

```sh
apt-get install bubblewrap                    # required; a corpus sweep refuses to run without it
touch src/main.rs && cargo build --release    # a stale binary is measured silently
scripts/ecosystem-sweep.py --prefix A --jobs <cores> --attempts 2
```

**Run it shard by shard and commit after each one.** Roughly 25 minutes per shard on 4 cores,
so a full corpus is 4+ hours — longer than an ephemeral container reliably lives. A shard that
finished is a shard whose records are worth keeping; a 4-hour run that dies at hour 3 has
produced nothing.

Three ways a running sweep dies, all seen:

- **The container restarts when the session goes idle.** Measured three times in one session
  (`uptime` reporting `up 0 min` right after a wake, every background process gone). `setsid`
  does not help — the machine, not the process group, is going away.
- **`git stash`** — including `git stash -u` to move the branch — **takes the untracked
  records the sweep is writing**. Never stash while a sweep runs.
- **The session's process group is cleaned up.** `setsid nohup … &` covers this one.

**So in a remote container, do not launch a sweep and go idle — it will not be there when you
come back.** Drive one shard *per turn*, waiting for it inside the turn (a background command
plus an `until ! ps aux | grep -q '[e]cosystem-sweep'; do sleep 20; done` loop keeps the
session active and the container alive), commit, and let a scheduled check-in start the next
one. That turns the restart from the thing that kills the sweep into the thing that paces it.
A 4-hour unattended corpus run is not available here; on the maintainer's box it is, which is
what ADR-0085 D9 assumes.

Restart from the shard after the last `done` line in the driver's log; already-measured
distributions are cheap to redo but not free.

## 2. Reading the results

```sh
scripts/ecosystem-sweep.py --rollup       # summary.json / summary.md / history.svg
```

**`history.tsv` takes one row per FULL sweep.** Appending a row for a partial corpus puts a
point on the KPI chart that is not comparable with the ones after it. Same reasoning for
`summary.*`: a rollup over a handful of distributions reads like a KPI and is not one.

### The counting trap — read this before quoting any impact number

A record's `load` map has **one entry per provided module**, not per distribution. A single
distribution with 32 modules produces 32 identical failure lines. Counting those as 32
distributions overstates a blocker by an order of magnitude, and it happened on the first
sweep: `Unknown role` was reported as 22 distributions when it was 5.

**Always aggregate into a `set` of distribution names, and say which unit you are quoting.**

```python
byfam = collections.defaultdict(set)          # set, not a counter
for p in glob.glob('ecosystem/dists/*/*.json'):
    r = json.load(open(p))
    for module, detail in (r.get('load') or {}).items():
        if detail not in ('ok', 'raku_also_fails'):
            byfam[family(detail)].add(r['dist'])   # ← the DIST, not the module
```

### Reading the three parity numbers

`assertion_parity` far **below** `file_parity` means the failing files die early rather than
drifting apart assertion by assertion — the gap is a few hard stops, not many small
incompatibilities. `blocked_load` being the largest bucket says the same thing. When both hold,
attack loading, not assertion semantics.

## 3. From a cluster to a filed issue

Work the ranking, but **file as soon as a repro is clean — never batch them for later**. The
container may not survive to "later", and a root cause with a repro is worth filing whatever
its rank; impact counts can be added as a comment when the sweep finishes.

1. **Reproduce outside the sandbox first.** Extract the distribution, build the `-I` list from
   `index.closure(dist)`, and run both interpreters by hand. A verdict that does not reproduce
   is a harness artifact, not a finding.
2. **Minimise, then build the neighbour table.** Cut until one line fails, then write down the
   *near misses that pass*. `sub f($x ($a, $b) where {…})` fails; sub-signature alone passes and
   `where` alone passes — that table is what names the bug and becomes the `t/` pin.
3. **Check whether the cluster is one cause or several.** `CSS::*` looked like one blocker
   across 8 distributions and was two, chained: an enum export tag (#7866) and `also is` in a
   `unit grammar` (#7867). Load each provided module separately to find where the chain breaks.
4. **Say when issues are ordered.** If A must be fixed before B's distributions can be
   re-measured, write that in both — otherwise the still-red ledger after the first fix reads
   as a failed fix.

### Two misleading errors this sweep produces

- **`Undeclared routine: Foo:ver` naming a package declarator** is a parse failure *inside*
  that package's body, backtracked out until the declarator re-read as a call. Bisect the body;
  the declarator is fine.
- **A `===SORRY!===` header is not a cause.** The reason is the next line. `first_error_line`
  skips the header for exactly this reason (pinned by `scripts/ecosystem_common.py --self-test`)
  — if 30 distributions all show the same parse message, suspect the extractor before believing
  they share a cause.

## 4. Issue shape

Label `todo:ticket` (`todo:deep` if it needs design). The body needs, in this order: the
minimal repro with **raku's and mutsu's output side by side**, the neighbour table, how many
**distributions** it blocks and which, and where it bites in real source. Say in "fix notes"
that the passing rows of the table are pins too — they are the control.

Never file against a Raku-org repository, only `tokuhirom/mutsu`.

## 5. Publishing

`scripts/gen-ecosystem-manifest.py` projects the records onto `site/content/ecosystem.json`,
and `pages.yml` regenerates it **at deploy time from whatever records have landed**. So a
partial sweep reaches the public page whether or not you commit the manifest — the coverage
figure the page leads with is what keeps that honest. Check it still says the truth after
changing the generator.
