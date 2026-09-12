# The ecosystem parity campaign is closed; the work moves per distribution

[#7785](https://github.com/tokuhirom/mutsu/issues/7785) — "check the test pass
rate of the zef module", filed 2026-09-09 — is closed. All five phases of
[ADR-0085](../../docs/adr/0085-ecosystem-testsuite-parity-measurement.md) landed
in three days, and what the issue asked for exists:

- every zef distribution's own test suite runs under **both** rakudo and mutsu,
  with rakudo as the denominator (a file rakudo does not pass cleanly is never
  charged to mutsu);
- the result is machine-readable **in the repository** — one JSON record per
  distribution under `ecosystem/dists/`, carrying the rakudo version, the mutsu
  commit, the dependency closure and the host that measured it;
- re-measurement is per distribution (`--only Dist::Name`), per shard
  (`--prefix A`), per status (`--status blocked_load`), or whole-corpus, locally
  or from GitHub Actions, and the corpus is re-measured **nightly**;
- users can see it: `site/ecosystem.html`, generated from the ledger;
- and the headline number is published with its history and chart — **41.1%**
  dist parity, 53.4% file, 63.1% assertion.

## Why close it rather than leave it open

A tracking issue is worth keeping while there is a thing to build. There no
longer is: the campaign's own deliverables are done, and #7785 had started to
accumulate the *other* kind of work — "make this distribution pass", "fix this
root cause" — which is exactly what it should not carry. One issue cannot track
1200 non-green distributions, and an open umbrella issue makes it look as though
something is waiting on a phase rather than on ordinary interpreter work.

So from now on the queue is picked one of two ways, both of which already exist:

- **by root cause** — `scripts/ecosystem-tickets.py` clusters the ledger and
  ranks clusters by how many distributions each affects; the fifteen largest are
  filed as `todo:*` issues (#7988, #7989, #7991-#7997, #7999-#8004), and a new
  sweep's new clusters are found by searching the tracker for their
  `eco-cluster: <id>`;
- **by distribution** — the
  [`ecosystem-dist-fix`](../../.agents/skills/ecosystem-dist-fix/SKILL.md) skill
  for a named one, or
  [`ecosystem-dist-roulette`](../../.agents/skills/ecosystem-dist-roulette/SKILL.md)
  for a uniform random draw with a lock on
  [#7884](https://github.com/tokuhirom/mutsu/issues/7884) so parallel agents do
  not collide.

Prefer a root cause when one covers several distributions — the largest single
cluster is 99 distributions, and one parse fix in `Terminal::Widgets::Widget`
unblocks 16. Prefer the uniform draw over picking a cheap-looking record: the
published figure is only honest if the sample is not chosen to flatter it.

PLAN.md's B4 bullet was rewritten to say that (it had been tracking phases), and
`docs/ecosystem-parity.md` now opens with "pick work, not phases" instead of a
phase status line.
