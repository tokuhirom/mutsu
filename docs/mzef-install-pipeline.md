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
| 3 | **Fetch** (download archive) | ⏳ | Single-dist `zef fetch` downloads `https://360.zef.pm/.../*.tar.gz` via the **curl**/**wget** backends (`Proc::Async` shell-out — no native TLS). Unblocked by #4615 + #4617. **The concurrent multi-candidate fetch that `install` drives still fails** (see "Current frontier") |
| 4 | **Extract** (untar) | ✅ | Untars the real dist after #4620 + #4622 + #4627 + #4635/#4641/#4642 |
| 5 | Build | ⬜ | not reached |
| 6 | Test | ⬜ | not reached |
| 7 | Install into site repo | ⬜ | not reached — but the **install→`use` bridge is already done** (`t/compunit-repository-for-name.t`), so once a dist reaches here it should be resolvable |

**So: ~4.5 of 8 phases.** Resolution, dependency collection, single-dist fetch
and extract all work against the live ecosystem. `install` now gets all the way
to fetching its 16 resolved candidates; the frontier is that those *concurrent*
fetches clobber each other's URL.

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

### Extract phase — three language bugs, all general (2026-07-16)

The "silent-fail → usage" was NOT an extract-specific problem: an exception
thrown inside `extract` is swallowed by mutsu's MAIN dispatch and surfaces as a
usage dump. Instrumenting the copy (`note`s down `Zef::Client::!extract` →
`Zef::Extract.ls-files`/`.extract`) found three ordinary language bugs, each
fixed generally:

- **#4620** — a **coercion-typed parameter** (`Str() $uri`) failed to dispatch a
  *native* target method. zef's `tar.extract-matcher(Str() $uri)` gets a
  `$candi.uri` `IO::Path`; `IO::Path.Str` is native, so binding threw
  `No such method 'Str' for invocant of type 'IO::Path'`. Fix: gate the
  "run the object's coercion method" branch on `class_has_user_method`, letting
  native methods fall through to the native dispatcher. Pin:
  `t/coercion-native-method-param.t`.
- **#4622** — **`IO::Path.relative($base)`** returned the *absolute* path when
  `$base` was not a literal ancestor. zef builds the `tar -C` target and staged
  archive path from `$archive.relative($tmp)` / `$extract-to.relative($cwd)`.
  Fix: implement raku's `abs2rel` (common-prefix drop + `..` ascent). Pin:
  `t/io-path-relative-abs2rel.t`.
- **#4627** — a **closure created inside a `.map`/`.grep` block** lost lexical
  capture of an outer free var when invoked through a callee with a same-named
  parameter. zef's `extract` maps over backends and, inside
  `lock-file-protect(IO() $path, &code)`, runs `start { $extractor.extract($path,
  …) }` — `$path` resolved to `lock-file-protect`'s `$path` param, so `tar`
  received the `.lock` file ("This does not look like a tar archive"). Root
  cause: the compile-time `authoritative_free_vars` propagation (#4510) does not
  reach a closure the inline map/grep path **re-compiles**. Fix: cascade the
  vouch at runtime via `Interpreter::frame_authoritative`. Pin:
  `t/closure-map-block-free-var-capture.t`.

### Dependency resolution — the bare-name shared-store collision (2026-07-17)

`zef install` aborted with `Invalid dependency specification: True`:
`Zef::Distribution.depends-specs` does `my $depends := system-collapse($.depends)`,
and when `system-collapse` returned `Nil` the bind produced **`True`** — the value
of the *caller's* (`Zef::Client`) unrelated `$!depends` flag.

Root cause: the cross-thread shared store (`shared_vars`) is keyed by **bare name**
and is **global to the process**. Every `start`/`Proc::Async` spawn makes
`clone_for_thread` migrate all env lexicals it can see into it, so zef's early
spawns left a `depends => True` entry behind. `GetLocal` treated a `Nil` slot as
"uninitialized — refresh from the shared store" and read that foreign entry by
bare name. `set_shared_var_sym` already masked the **write** side on
`thread_redeclared_vars` (a re-declared name is a fresh binding shadowing the
migrated one); the read path did not, and that asymmetry *was* the bug. It
explains every symptom: `:=`-only (a `=` wraps the value in a `Scalar` container,
so the slot is a `ContainerRef` and returns before the Nil branch), `Nil`-only
(the fallback is gated on `is_nil()`), and name-sensitive (only names that some
spawn actually migrated collide). Fixed by gating the read on the same
`thread_redeclared_vars` mask. Pin: `t/shared-var-nil-redeclared-mask.t`.

Repro (`victim` prints `Bool::True` on the second call before the fix):

```raku
sub nil-returner() { Nil }
sub spawner() { my $depends = True; await start { $depends }; }
sub victim()  { my $depends := nil-returner(); say $depends.raku; }
victim();    # Nil
spawner();   # migrates `depends => True` into the global shared store
victim();    # was Bool::True, now Nil
```

## Current frontier — concurrent fetch clobbers the URL

With dependency resolution fixed, `zef install Test::META` resolves **15
prereqs / 16 candidates** and reaches the fetch phase for all of them — then
every fetch fails. The `--debug` trace shows why: the candidates are fetching
**each other's URL**.

```
[Test::Async]     Fetching https://360.zef.pm/J/SO/JSON_OPTIN/7ada0a9….tar.gz with plugin: …wget
[JSON::Unmarshal] Fetching https://360.zef.pm/J/SO/JSON_OPTIN/7ada0a9….tar.gz with plugin: …wget
[META6]           Fetching https://360.zef.pm/J/SO/JSON_OPTIN/7ada0a9….tar.gz with plugin: …wget
```

Six unrelated dists all request `JSON::OptIn`'s archive, so the downloaded file
never matches the candidate and each is reported `Fetching [FAIL]`. Single-dist
`zef fetch Test::META` still works (exit 0) — it is the **concurrent** fetch that
`install` drives (`Zef::Client.!fetch` hypers over candidates) that breaks.

### Next session starts here

The shape — one lexical's value leaking across sibling concurrent tasks — is the
same family as the bug fixed above, and quite possibly the *same* mechanism:
`shared_vars` is a **process-global, bare-name** map, so N concurrent
`start`-block frames each holding their own `$uri`/`$url` lexical all collide on
one key, and last-writer-wins.

1. Instrument `Zef::Service::Shell::wget`/`curl` `.fetch` (per the tooling
   section below) to print the `$uri` it receives vs the one it shells out with —
   confirm where the value diverges.
2. Reproduce in isolation first: several `start` blocks, each with a same-named
   lexical bound from its own argument, read after a sibling has spawned. If that
   reproduces, the fix belongs in the shared store's keying, not in zef.
3. Note that per-name masking (`thread_redeclared_vars`) is a *band-aid on the
   band-aid*: the real defect is that a thread-shared store keyed by bare name
   cannot distinguish two unrelated lexicals of the same name. If the fetch bug
   is the same mechanism, prefer fixing the keying (scope-qualified keys) over
   adding another mask — and record it as an ADR, since it touches the dual-store
   campaign (PLAN §6 / `project-slice-f-reverse-sync-campaign`).

## How to run zef under mutsu (tooling)

- **zef checkout (known-good vendoring):** `/home/tokuhirom/work/mutsu/tmp/zef-dbg`
  (`bin/zef` = `use Zef::CLI` launcher; `lib/`; `resources/`; version 1.1.3).
  Debian's `/usr/share/perl6/debian-sources/raku-zef/` lacks `bin/` — do not use it.
- **Invoke:** `target/release/mutsu -I <zef-dbg>/lib <zef-dbg>/bin/zef <args>`
  (`--debug` adds `DBG …` traces; `DBG pop-*` = populate progress).
- **Instrumenting the extract path:** the out-of-repo `zef-dbg` cannot be edited
  by the tool sandbox, so copy it into the scratchpad and add `note`s there, then
  point `-I` at the copy: `cp -r /home/tokuhirom/work/mutsu/tmp/zef-dbg
  <scratchpad>/zef-instr` → `target/release/mutsu -I <scratchpad>/zef-instr/lib
  <scratchpad>/zef-instr/bin/zef fetch Test::META`. Useful note sites:
  `Zef/Client.rakumod` `!extract`, `Zef/Extract.rakumod` `ls-files`/`extract`,
  `Zef/Service/Shell/tar.rakumod` `extract` (print the `archive`/`-C` it hands
  `tar` — a `.lock` suffix on the archive is the closure-capture leak).
- Use a **release** build for anything touching populate (debug is minutes).
