# mzef install pipeline — progress tracker

**Purpose of this file:** make "how far along is mzef?" legible at a glance, and
give the next session an exact starting point. mzef = mutsu shipping the real
[Zef](https://github.com/ugexe/zef) as its package manager (PLAN.md §1 B2). The
strategy is to **run the unmodified real zef under mutsu** and fix each mutsu
compatibility bug the pipeline hits — zef is the strongest compat north star.

> The headline finding (2026-07-16): **network fetch needs no native TLS in
> mutsu.** zef shells out to the system `curl`/`wget`, which handle TLS; mutsu
> drives that via `Proc::Async`. So the old assumption in PLAN §1 B2 ("robust
> async TLS is the biggest prerequisite") was wrong. The frontier is the
> **install pipeline phases**, each a functional (not performance) problem.

## The end goal

`mzef install <dist>` on a fresh machine: resolve → **fetch** → **extract** →
**build** → **test** → **install** a real dist from the fez ecosystem into the
mutsu site repo (which `use` then resolves). "A dist installs and is then
`use`-able" is the definition of done for this pipeline.

## Pipeline status

Legend: ✅ works · ⏳ in progress · ⬜ not yet reached · 🔒 blocked

| # | Phase | Status | Evidence / blocker |
|---|-------|--------|--------------------|
| 0 | CLI load + command dispatch | ✅ | `zef --version` → 1.1.3; `--help` prints usage |
| 1 | Ecosystem index (populate) | ✅ | fez index parsed: **9260 keys / 7648 dists**, ~6.5s release |
| 2 | Resolve / find candidate | ✅ | `zef info Test::META` → full Identity/Source/Description |
| 3 | **Fetch** (download archive) | ✅ | `zef fetch` downloads `https://360.zef.pm/.../*.tar.gz` via the **curl** backend (`Proc::Async` shell-out — no native TLS). Unblocked by #4615 + #4617 |
| 4 | **Extract** (untar) | ⏳ | Reached; **silent-fails in the full zef path** (see "Current frontier"). All components work in isolation |
| 5 | Build | ⬜ | not reached |
| 6 | Test | ⬜ | not reached |
| 7 | Install into site repo | ⬜ | not reached — but the **install→`use` bridge is already done** (`t/compunit-repository-for-name.t`), so once a dist reaches here it should be resolvable |

**So: 3.5 of 8 phases.** Everything through *fetch* works end-to-end against
the live ecosystem; *extract* is the active edge.

## Fixes that got us here (this campaign)

- **#4615** — `my Candidate @fetched = @candidates.hyper(...).map(...)` (zef's
  `!fetch`) failed with "expected Candidate, got HyperSeq": a `HyperSeq`/
  `RaceSeq`/`Slip` assigned to a typed array was type-checked whole instead of
  per-element. Fixed both the declaration (`TypeCheck` op) and assignment
  (`coerce_typed_container_assignment`) paths. Pin: `t/hyperseq-typed-array.t`.
- **#4617** — `Proc::Async.start(:cwd, :ENV)` ignored both named args, so zef's
  `tar`/`git`/`curl` shell-outs (which run with `:cwd($archive.parent)` and a
  *relative* archive path) ran in the wrong directory and silently failed. Now
  applied via `Command::current_dir` / `env_clear`+envs. Pin:
  `t/proc-start-cwd-env.t`.

## Current frontier — extract silent-fails in the full zef path

`zef fetch <dist>` now runs: `Searching → Found → Fetching [OK] → Extracting:` →
then **prints usage and exits, producing no extracted files**, with **no
exception reaching zef's `proto MAIN` `CATCH`** (`Zef/CLI.rakumod:329`) — so the
failure is swallowed on a worker/detached context, not surfaced.

What has been ruled out (each works in isolation under mutsu):

- the **tar backend itself** — `Zef::Service::Shell::tar.ls-files`/`.extract`
  return the 9 files and extract `Test-META-0.0.20` correctly (needs real
  `:stdout`/`:stderr` Suppliers);
- the **extract concurrency core** — `$s.Supply.act` tap + `start { try … }` +
  `await Promise.anyof($todo, $time-up)` extracts correctly;
- `Proc::Async` with `:ENV(%*ENV)` + `:cwd` + a relative `tar` path.

So the bug is in the **integration**: `Zef::Client::!extract`
(`Zef/Client.rakumod:591`, `my Candidate @extracted = eager gather for …`) →
`Zef::Extract.extract` (`Zef/Extract.rakumod:115`: `lock-file-protect` +
`start { try … }` + `Promise.anyof` + `self!extractors($path).map(…)`), under
real `Candidate`/backend objects.

### Next session starts here

1. Instrument the extract path to find where the silent fail is. Editing the
   out-of-repo `zef-dbg` is blocked by the tool sandbox, so **work on a copy**
   (a scratchpad copy `zef-dbg-instr` was used this session): add `note`s inside
   `Zef::Client::!extract` and `Zef::Extract.extract` around `lock-file-protect`,
   `self!extractors`, and the `start`/`await` to see which returns `Nil`/throws.
2. Likely suspects (from the ruled-out list): `lock-file-protect` not running
   its block, `self!extractors($path)` selecting no backend for a `Candidate`'s
   uri, or the `eager gather` swallowing a per-candi exception.
3. Fix the mutsu bug generally, add a `t/` pin (as with #4615/#4617), PR it, and
   move to the next phase (build).

## How to run zef under mutsu (tooling)

- **zef checkout (known-good vendoring):** `/home/tokuhirom/work/mutsu/tmp/zef-dbg`
  (`bin/zef` = `use Zef::CLI` launcher; `lib/`; `resources/`; version 1.1.3).
  Debian's `/usr/share/perl6/debian-sources/raku-zef/` lacks `bin/` — do not use it.
- **Invoke:** `target/release/mutsu -I <zef-dbg>/lib <zef-dbg>/bin/zef <args>`
  (`--debug` adds `DBG …` traces; `DBG pop-*` = populate progress).
- **Repros built this session (in `tmp/`):** `proc-async-curl.raku` (fetch via
  curl), `tar-ls-files.raku` (tar backend), `zef-tar-direct.raku` (tar plugin
  ls-files/extract with real Suppliers), `extract-concurrency.raku` (the
  `start`+`Promise.anyof` core).
- Use a **release** build for anything touching populate (debug is minutes).
