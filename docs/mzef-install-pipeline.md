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
| 3 | **Fetch** (download archive) | ✅ | Downloads via the **curl**/**wget** backends (`Proc::Async` shell-out — no native TLS). Unblocked by #4615 + #4617; the **concurrent** multi-candidate fetch `install` drives by #4658 (ADR-0010) — all 16 of Test::META's candidates now fetch their own archive |
| 4 | **Extract** (untar) | ✅ | Single-dist: #4620 + #4622 + #4627 + #4635/#4641/#4642. Concurrent: fixed by restoring the #4650 `thread_redeclared_vars` gates on top of ADR-0010 (the "extract-matcher rejects the archive" symptom was a child `my` re-declaration clobbering the parent's staged path through the lineage chain) |
| 5 | Build | ✅ | reached; a pure-Raku dist has nothing to build and passes through |
| 6 | Test | ⏳ | The phase RUNS: `zef install JSON::OptIn` (no `--/test`) does `Testing [OK] → Installing`, and `zef install Test::META` runs all 16 dists' suites concurrently. But **13/16 suites FAIL** — see "Test-phase frontier" below |
| 7 | Install into site repo | ✅ | **a dependency-free dist installs and is then `use`-able**; a dependency-ful one too: `zef install --/test Test::META` installs **all 13 dists** end-to-end |

**So: the whole install pipeline works, including for a dependency-ful dist.**
A real dist from the live fez ecosystem downloads, extracts, installs into the
mutsu site repo, and `use` resolves it:

```
$ mutsu -e 'use JSON::OptIn'        # Could not find JSON::OptIn in: ...
$ mutsu -I <zef>/lib <zef>/bin/zef install --/depends --/test JSON::OptIn
===> Installing: JSON::OptIn:ver<0.0.2>:auth<zef:jonathanstowe>
$ mutsu -e 'use JSON::OptIn; say "LOADED"'
LOADED
$ mutsu -I <zef>/lib <zef>/bin/zef install --/test Test::META
===> Installing: JSON::Fast … (13 dists) … Test::META:ver<0.0.20>
```

**Pick a non-bundled dist when verifying this.** mutsu bundles JSON::Fast (and
others), so `use JSON::Fast` succeeds on a clean HOME with no install at all and
proves nothing. `JSON::OptIn` is not bundled.

**The whole load chain works too** (as of #4661–#4665): `use Test::META` and
every dist below it — JSON::Unmarshal, JSON::Marshal, JSON::Class
(jonathanstowe), License::SPDX, META6, URI — load, and `URI.new(...).host`
returns. The frontier is the **Test phase** ("Test-phase frontier" below).

> **Verification trap:** `use Test::META; say "LOADED"` proves NOTHING on its
> own — `use_module` has a compat branch that silently succeeds for any
> missing `Test::*` module (runtime_module.rs). Verify with a symbol the
> module actually exports: `::("&meta6-ok")` must not be an error. The
> non-`Test::` dists in the chain (`use META6` etc.) fail honestly.

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

### Install phase — the silent no-op (2026-07-17)

`zef install` printed `===> Installing: …` and exited **0** having installed
nothing loadable. Three false leads worth not repeating:

- `.can("install")` returns **False** on `CompUnit::Repository::Installation` —
  a false negative. `.can` does not know natively-dispatched methods; `install`
  *was* implemented and *was* being called.
- `@curs` was **not** empty (the `DBG cli-6 to=1` trace) — `===> Installing:` is
  printed *before* the `for @curs` loop in `Zef::Client.install`, so an empty
  `@curs` was the obvious suspect and the wrong one.
- The install *did* write a file. `find $HOME -type f | head` showed only the
  `.zef` store and precomp cache, which read as "nothing written" — the dist
  JSON was further down the list.

The real defect (#4655): install joined the dist's **`prefix` attribute** onto
each `provides` address to find the sources, but `Zef::Distribution::Local` has
`$.path`/`$.IO` and **no `prefix`**, so the join produced a relative path that
never existed and `std::fs::copy(..).ok()` swallowed it — the metadata recorded
`provides` entries naming files it had never copied. Fixed by resolving each
address through the distribution's own `.content($name-path)` (the API every
Distribution implements, S22), with the `prefix` join kept as a fallback.
Pin: `t/cur-install-content-api.t`.

## Solved — the concurrent-fetch collision (2026-07-17, ADR-0010)

Fixed by scoping the cross-thread store to a spawn lineage instead of one
process-global bare-name map: sibling hyper workers no longer share a `uri` key.
All 16 candidates now fetch their own archive. See
[ADR-0010](adr/0010-cross-thread-lexical-sharing-scope.md); the analysis that
found it is kept below.

## Load frontier — CLOSED (2026-07-17, #4661–#4665)

`zef install --/test Test::META` completes (16 candidates resolve, fetch,
extract, **13 dists install**), and the whole load chain is now fixed:
**`use Test::META` loads**, as do JSON::Unmarshal, JSON::Marshal, JSON::Class
(jonathanstowe), License::SPDX and META6. Load-frontier fixes (each: minimal
repro → general fix → `t/` pin):

- ~~`-> Mu \type { ... }` typed sigilless pointy param~~ (#4661) — unblocked
  `use JSON::Unmarshal`'s `subset ... where` lambdas.
- ~~sibling-role short-name composition inside a module~~ (#4661) — unblocked
  JSON::Unmarshal's `CustomUnmarshaller` role family; JSON::Marshal loads.
- ~~`use` dist selectors discarded / first-found dist loaded~~ (#4662) — two
  installed dists provide `JSON::Class` (zef:jonathanstowe vs zef:vrurg);
  resolution now filters by `:auth`/`:ver`/`:api` and picks the highest
  version. Pin: `t/use-dist-selectors.t`.
- ~~`role Name:ver<...>:auth<...>[SIGNATURE]`~~ (#4662) — adverbs before the
  parametric signature failed to parse (jonathanstowe JSON::Class line 121).
  Pin: `t/role-decl-adverbs-signature.t`.
- ~~Attribute role-mixin dropped by `trait_mod:<is>`~~ (#4663) — three defects
  (ephemeral Attribute object; short role name degrading `does` to a boolean
  check that REBOUND `$a`; imported multi candidates from two modules
  colliding on one key). Pin: `t/attr-trait-role-mixin.t`. Unblocked
  License::SPDX.
- ~~class-body trait multis invisible to nested classes~~ — during a nested
  class's registration, multi lookup probed only current_package + GLOBAL, so
  `class META6 { multi sub trait_mod:<is>(...) ...; class Support { has ...
  is specification(Optional) } }` failed; the trait dispatch now walks up the
  package chain. Pin: `t/class-body-trait-multi.t`. Unblocked META6 and with
  it **Test::META**.
- ~~a named ARRAY param matched a scalar value~~ — `:@specification!
  (Optionality $o, Version $v)` out-dispatched `Optionality :$specification!`
  for a single enum value and died in the destructure. Named `@`/`%` params
  now require Positional/Associative arguments. Same pin.

- ~~`use URI; URI.new(...).host` return type check~~ (#4665) — URI's
  `subset Host of Str where /regex/` passed `~~` but failed every `--> Host`
  return: the return check's own predicate evaluator only understood callable
  predicates. It now delegates to the same subset matcher `~~` uses. Pin:
  `t/subset-regex-return-check.t`.

## Test-phase frontier (2026-07-18 refresh)

**Per-suite standing (2026-07-18, after #4735 / #4738 / #4747 / #4756;
staged-dist `prove` sweep with the full MUTSULIB chain,
`tmp/e2e-suites.sh`): 10 PASS / 1 FAIL.** PASS: JSON::OptIn, JSON::Name,
JSON::Unmarshal (11 files / 101 tests), JSON::Marshal (14/85), JSON::Class
(6/31), URI (14/222), META6 (9/969), License::SPDX (2/729), Test::META
(3/26, #4756). FAIL: **JSON::Fast** only (native-JSON fidelity remnants;
full parity is likely unnecessary for the pipeline). Within JSON::Fast,
three PRs (#4762 AdditionalContent + strict grammar; #4765 options parity:
`:immutable`/List+Map decode, `:enums-as-value`, `$*JSON_NAN_INF_SUPPORT`,
import-list defaults `<immutable !pretty>`, `&from-json`/`.&from-json`
code-object dispatch, uppercase surrogate escapes, Duration-as-Num; the
numerator/denominator-shadow PR: those builtins no longer claim every
invocant, so the Rational role prelude's attr accessors work and a punned
Rational serializes numerically) brought the suite from 3/14 to **11/14
files green**; remaining: 07-datetime (`augment class DateTime`),
12-assocpositional (Positional+Associative instance serialization),
14-comments (JSONC). Known adjacent gap (not needed by the suite): an
UNparameterized `Rational.new(6,4)` still fails binding (`expected NuT,
got Int` — role type-param defaults `::NuT = Int` unresolved in method
signatures).
Test-Helpers ships no `t/` of its own (covered by mutsu's local
`t/test-util-*.t`).

The #4747 session's two generic fixes (License::SPDX 0/2 → 2/2): a trait
argument with whitespace after the opening paren
(`is unmarshalled-by( -> $d { ... })`) was silently dropped by the `has`
parser, and recursive re-binding of a same-named typed container
(`my @ret := Array[T].new` in JSON::Unmarshal's `_unmarshal` recursion)
clobbered the name-keyed element-type constraint — element checks and `.of`
now prefer the metadata embedded in the value. Pins:
`t/attr-trait-paren-space.t`, `t/typed-bind-recursion.t`.

History (2026-07-17): `zef install Test::META` (tests ON) runs all 16
dists' test suites concurrently; the baseline was **13/16 FAIL** (OK:
JSON::OptIn; the debug-build run also overran a 570s timeout — use release
for E2E). The 2026-07-17 (late) session root-caused the dominant failures
— none were environmental; all four were ordinary mutsu bugs, each fixed
generally:

- ~~`use-ok` never did a real load~~ (the "dominant symptom"): mutsu's
  `use-ok` probed `lib_paths` for `Module/Name.rakumod` as literal files —
  it could not see `inst#` installation repos, and the staged dist zef
  tests against is exactly that, so every suite's `t/010-use.t` failed
  while interactive `use` worked. Fixed: `use-ok` now delegates to the
  real `use_module()` loader (Rakudo EVALs `use $module`). Pin:
  `t/use-ok-real-load.t`.
- ~~Test::Async (vrurg) `X::Redeclaration: '$self'`~~ (#4669): TWO
  `::?CLASS` param bugs — a `\`-sigilless param after the pseudo-type fell
  into the bare-invocant branch and created a second `self` param
  (`create-suite(::?CLASS:D: ::?CLASS:U \suiteType = self.WHAT)`), and a
  non-invocant `::?CLASS` constraint was type-checked as a literal string
  so it never bound. Pin: `t/class-pseudo-type-param.t`. **Test::Async's
  next blocker is custom Metamodel HOW inheritance**
  (`'Test::Async::Metamodel::BundleHOW' cannot inherit from
  'Metamodel::ParametricRoleHOW' because it is unknown`) — a real MOP
  feature, campaign-sized; not started.
- ~~a mixed-in role's composed roles invisible to `.does`/`~~`~~:
  `role NamedAttribute does JSON::OptIn::OptedInAttribute` mixed into an
  attribute answered False for the composed role, failing JSON::Name's
  `t/020-trait.t` `does-ok`. Class composition already walked the role
  graph; only the mixin path lost it. Pin:
  `t/mixin-role-transitive-does.t`.
- ~~map-block `my` clobbered a caller lexical → JSON::OptIn dropped from
  prereqs~~: the inline map/grep/first paths run the block's body directly
  in the enclosing frame's env, so `provides-specs`'s
  `.map({ my $spec = ...; $spec })` overwrote `provides-spec-matcher`'s
  `$spec` PARAMETER — `contains-spec` then matched the dist against itself
  and `zef install JSON::Name` silently skipped installing JSON::OptIn
  (cache-cold first call only, which made it look nondeterministic). Fixed
  by recording each block's own `my` declarations
  (`CompiledCode::my_declared_sym`) and masking them from the
  closure-exit/inline writeback (a declared name that is also a free var
  keeps its writeback). Pin: `t/map-block-my-shadow-leak.t`.

~~Re-run the full `zef install Test::META` E2E after these land to get the
new pass/fail count.~~ Done 2026-07-18 — see the per-suite standing at the
top of this section (10 PASS / 1 FAIL).

**Test::Async is out of scope for this frontier** (PLAN.md §1 B5): its
blocker is custom Metamodel HOW inheritance
(`Metamodel::ParametricRoleHOW`), a campaign-sized MOP feature that the
mzef dependency chain does not need.

Still open (load-side leftovers):

- **CALLING a named-array destructure candidate** (`is specification([...])`)
  still fails in binding (`Calling trait_mod:<is>(Any) will never work ...`,
  then binds the whole Array to the first destructure param). Not used by
  META6 itself; noted in `t/class-body-trait-multi.t`.
- **vrurg JSON::Class (`use v6.e.PREVIEW`) line 40 parse error** — no longer
  in the default chain (the selector fix routes to jonathanstowe's), but the
  dist is installed and still cannot load.
- **`R[:b]` named-arg role parameterization binds the Pair, not its value**
  (mutsu prints `b => True` where raku prints `True`) — noticed while fixing
  the role-adverb parse; harmless for truthiness uses like
  `JSON::Class[:opt-in]` but wrong.

### Resolved: the extract matcher rejection was the shared-store parent-child clobber

The previous frontier — `Enabled extracting backends [git tar unzip path] don't
understand /tmp/.zef.…/….tar.gz` on the concurrent path only — disappeared with
the ADR-0010 gate fix (restoring the #4650 `thread_redeclared_vars` gates). The
suspected mechanism held: nothing was at the staged path because a hyper
worker's `my` re-declaration (e.g. its `$tmp`/`$stage-at`/pointy-block bindings
inside a nested `start`) wrote through the lineage chain to an ancestor's
same-named lexical, corrupting another worker's staging state. Same defect
class as the day14 `-> $parsed` clobber; see ADR-0010's "mask stays" section.

## Superseded analysis — concurrent fetch clobbers the URL

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

What the trace pins down: the `[Test::Async]` / `[META6]` prefix comes from
`$candi` and is **correct** for every line, while the `$uri` in the same log
message is **wrong**. Both are read at `Zef::Fetch.fetch`:

```raku
method fetch(Candidate $candi, IO() $save-to, …) {
    my $uri      = $candi.uri;             # <- this is what diverges
    my @fetchers = self!fetch-matcher($uri).cache;
    …
    my $got := @fetchers.map: -> $fetcher {   # the closure that logs $uri
        $logger.emit({ … message => "Fetching $uri with plugin: {$fetcher.^name}" });
```

So `$candi` (a parameter) stays per-thread while `$uri` (a `my` scalar in the
same, concurrently-entered method frame) does not. Each fetch drives
`react`/`Proc::Async` under it, so `clone_for_thread` runs while several frames
of this method are live.

### Mechanism — confirmed (2026-07-17)

It **is** the bare-name shared store, and the missing ingredient is a **`start`
nested inside a hyper worker**:

1. `exec_hyper_race_parallel` calls `clone_for_thread()` per batch thread, which
   migrates the parent env's lexicals into `shared_vars` **by bare name**.
2. Each batch thread then runs the block, whose body does `my $uri = …` and
   `start { … }`. That inner `start` calls `clone_for_thread()` **again**, from
   the batch thread — migrating *its* `candi`/`uri` into the very same
   **process-global** map (`shared_vars` is one `Arc<RwLock<HashMap<String, …>>>`
   handed to every clone).
3. All N batch threads do this concurrently on the same keys. Last writer wins,
   and the inner `start` block resolves `$uri`/`$candi` by name — getting the
   winner's values.

Minimal repro (raku prints each letter with its own value; mutsu prints one
worker's pair repeatedly — e.g. `F:u-F D:u-D F:u-F C:u-C E:u-E F:u-F`):

```raku
sub matcher($u) { 1 }
sub fetch($candi) {
    my $uri = "u-$candi";
    matcher($uri);
    my &code = -> {
        my $todo = start { "$candi:$uri" };
        await $todo;
        $todo.result;
    };
    code();
}
say (<A B C D E F>.hyper(:batch(1), :degree(5)).map: -> $candi { fetch($candi) }).join(' ');
```

A sibling variant — the same shape but with the closure passed to a callee
(`lock-file-protect("$candi.lock", -> { … })`, as zef actually writes it) — fails
differently: `$todo` itself comes back `Any`, so `.result` dies with
`No such method 'result' for invocant of type 'Any'`. Both are the same
collision; only the key that loses differs.

Note what is **not** required, contrary to the obvious guesses: the call-arg
vouch veto (`matcher($uri)`) is **not** needed (the callee variant reproduces
without it), and neither `hyper` alone nor `start` alone reproduces — a plain
`hyper.map` calling a method that declares `my $uri` and shells out via
`Proc::Async` **passes**, because without a nested `start` there is no second
migration.

### Next session starts here

The fix is the shared store's **keying**, and it is architectural — write an ADR
first. The defect is that a thread-shared store keyed by bare name and global to
the process cannot distinguish two unrelated lexicals of the same name; sibling
hyper workers are *unrelated* and must not share, while a `start` nested inside
one worker *must* share with that worker.

- Per-name masking (`thread_redeclared_vars`, extended in #4650 to the read side)
  is a band-aid on a band-aid and does **not** help here: each batch thread has
  its own `Interpreter` and therefore its own mask, but they all share the one
  global map.
- The shape to evaluate is a **per-lineage store**: `clone_for_thread` gives the
  child a store that inherits the parent's entries and flows writes back on join,
  instead of `Arc::clone`-ing one process-wide map. Weigh it against the existing
  parent↔child sharing tests (`t/cross-thread-shared-var-writeback-coherence.t`,
  `t/concurrent-shared-cell.t`, the 148 thread/shared/atomic `t/` files) — those
  pin the sharing that must survive.
- This sits squarely in the dual-store campaign (PLAN §6 /
  `project-slice-f-reverse-sync-campaign`); coordinate with it rather than
  bolting on a third mechanism.

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
