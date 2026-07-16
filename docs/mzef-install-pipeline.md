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
| 4 | **Extract** (untar) | ⏳ | Reached the real `tar -xvf` call with correct paths after #4620 + #4622 + #4627. One **call-arg sub-case** of the closure-capture bug still misroutes `$path` → `.lock` (see "Current frontier") |
| 5 | Build | ⬜ | not reached |
| 6 | Test | ⬜ | not reached |
| 7 | Install into site repo | ⬜ | not reached — but the **install→`use` bridge is already done** (`t/compunit-repository-for-name.t`), so once a dist reaches here it should be resolvable |

**So: ~3.7 of 8 phases.** Everything through *fetch* works end-to-end against
the live ecosystem; *extract* is one narrow language bug (the call-arg vouch
veto) away from untarring the dist.

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

## Current frontier — extract's remaining call-arg sub-case

With #4620 + #4622 + #4627, `zef fetch Test::META` reaches the real
`tar -xvf … -C …` call with the **correct** archive/extract paths in the common
case. One narrower zef-specific sub-case of the #4627 closure bug remains and
still makes `tar` receive the `.lock` path:

`Zef::Extract.extract` binds `my $path := $candi.uri`, then passes `$path` as a
**call argument** — `self!extractors($path)` (the map invocant) and
`$extractor.extract($path, …)` (in the `start` block). mutsu's compile-time
vouch analysis conservatively **vetoes** any call-arg-source from
`authoritative_free_vars` (an `is rw` param *could* write it back — see
`own_call_arg_sources` in `compute_free_vars`). So `$path` is never vouched, the
#4627 runtime cascade never starts for it, and the `start` block's `$path` again
degrades to `lock-file-protect`'s `$path` param.

Minimal repro (fails; raku prints `(A:REAL B:REAL)`):

```raku
sub other($p) { 1 }
sub callee($path, &code) { code() }
sub extract($candi) {
    my $path := $candi;
    other($path);                                   # the call-arg veto trigger
    <A B>.map(-> $b { callee("$path.lock", -> { await start { "$b:$path" } }) });
}
say extract("REAL");                                # mutsu: (A:REAL.lock B:REAL.lock)
```

### Next session starts here

The **sound** fix (per ADR-0001's by-value-vs-cell guidance): a captured free var
that is a call-arg-source (potentially rw-mutated) should be **boxed into a
shared `ContainerRef` cell** rather than left a by-value snapshot. A `ContainerRef`
capture is already overwrite-installed at closure entry
(`call_compiled_closure_with_topic`, the `ValueView::ContainerRef` arm ~line 233)
AND tracks the live value, so it is immune to a same-named caller param and stays
correct even under an actual `is rw` write-back.

1. In `compute_free_vars` (`src/opcode.rs`), extend the `needs_cell` analysis:
   a free var that a nested closure captures AND that is in `own_call_arg_sources`
   should be added to `needs_cell` (its declaring frame boxes it). Today only
   *mutated* captures are boxed (`self_mutated`/`free_writes`); this adds the
   call-arg-source captured case.
2. Verify the minimal repro above prints `(A:REAL B:REAL)`, then re-run the full
   `zef fetch Test::META` — extract should now untar `Test-META-0.0.20` and the
   pipeline advances to **build**.
3. Watch for over-boxing regressions (perf + `=:=` identity / by-value
   expectations) via `make test`; boxing a readonly call-arg var is sound but
   touches a common pattern. Add a `t/` pin, PR it.
4. Then move to phase 5 (**build**): re-run `zef fetch`/`zef install --/--build`
   and instrument `Zef::Client.build` → `Zef::Service::Shell::DistributionBuilder`
   the same way.

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
