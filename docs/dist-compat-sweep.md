# Real-distribution compatibility sweep — dashboard

The single ledger of **which real fez-ecosystem distributions actually run under
mutsu, and why the rest fail**, tracked by root cause. Used to decide which
general mutsu fix would unblock the most real dists at once — the batteries /
compatibility counterpart of [`TODO_roast/BLOCKERS.md`](../TODO_roast/BLOCKERS.md).

## Why this file exists

[`docs/ecosystem-guts-dependency-survey.md`](ecosystem-guts-dependency-survey.md)
established that **~81% of the ecosystem is pure Raku** (not compiler-guts
blocked). But that is a *signal scan* — it explicitly does **not** mean "runs on
mutsu". This sweep is the **execution** counterpart: it downloads each dist,
points mutsu at the dist's own `lib`, and tries to `use` every module the dist
`provides`, bucketing the outcome. It answers the question the guts survey could
not: **of the reachable ~90%, how much runs today, and what are the top blockers?**

The headline finding (first run, 2026-07-19): among the pure-Raku dists whose
deps are satisfiable, **only about half load cleanly**; the other half hit a
genuine mutsu parse/runtime bug on `use` alone. So real-dist compatibility, not
roast, is the productive frontier for widening the batteries base (PLAN.md §1 B4).

## ⚠️ Sandbox — this runs untrusted code

`use <module>` on a real fez dist **executes arbitrary code** (BEGIN/CHECK
phasers, load-time side effects), and the planned Level-2 test-suite runs execute
even more. Running that unconfined on a dev machine is unsafe. The sweep therefore
runs **every mutsu invocation inside a [bubblewrap](https://github.com/containers/bubblewrap)
(`bwrap`) sandbox** by default:

- **No network** (`--unshare-all` includes a fresh, empty network namespace — the
  code cannot phone home or attack the LAN). Verified: `curl` inside exits 6.
- **Read-only filesystem** (`--ro-bind / /`) — the code cannot modify the repo,
  the binary, or anything on the host. Verified: `spurt` outside HOME → `os error 30`.
- **Throwaway HOME** — a tmpfs is mounted over an isolated HOME dir, so precomp /
  site-repo / any writes land in RAM and vanish; nothing reaches the host.
- **Own PID namespace + `--die-with-parent`** (fork bombs are contained and reaped)
  and **rlimits** (`ulimit -v`/`-u`: address-space and process-count caps).

Downloading and extracting the tarball happen **outside** the sandbox (in Python);
only the mutsu run — which needs no network — is confined. `bwrap` is required by
default; `--sandbox none` disables it and **runs untrusted dist code unconfined —
do not use it** except against dists you already trust.

## How to regenerate

```sh
# release build recommended; needs ~/.zef/store/fez/fez.json (any mzef/zef op populates it)
# sandboxed by default (bwrap); install: apt-get install bubblewrap
MUTSU_BIN=target/release/mutsu scripts/dist-compat-sweep.py --n 60 --seed 42 --timeout 25
# → tmp/dist-compat-sweep.tsv (per provided-module rows) + a bucket summary
```

- `--only Dist::Name` sweeps specific dists (repeatable); use it to re-check one
  after a fix.
- `--site-repo <HOME>` runs with that HOME's site repo on the path, so deps
  installed by a prior `mzef install` collapse `missing_dep` into a real verdict.
- `--include-guts` / `--include-native` un-skip those axes.
- `--sandbox {auto,bwrap,none}` (default `auto` = bwrap if present).
- Downloads are cached under `~/.cache/mutsu-dist-sweep/`.

**This sweep only exercises `use <module>` (a load/compile smoke test).** A dist
that loads can still fail at runtime or in its own test suite — that is a deeper
level to be added later (run the dist's `t/`/`.rakutest` with deps installed).
`load_ok` here means "parses, compiles, and loads", not "passes its tests".

## Buckets

| Bucket | Meaning | Reachable by |
|---|---|---|
| `load_ok` | every provided module loads | (already works) |
| `missing_dep` | `Could not find <X>`, X is another dist | install X (dep sweep) — **not a mutsu bug** |
| `parse_error` | `===SORRY!===` / `Confused` / parse failure | parser fix |
| `runtime_error` | other non-zero exit / load-time exception | a general mutsu bug |
| `panic` | Rust panic | VM/interpreter bug (**top priority**) |
| `timeout` | exceeded the per-module timeout | hang / perf bug |
| `skip_guts` / `skip_native` | carries a guts / NativeCall signal | separate axes (guts survey / NativeCall) |

## Latest run — n=60, seed 42 (2026-07-19, sandboxed via bwrap)

| Bucket | Count | % of 60 | % of executed pure (48) |
|---|---|---|---|
| `load_ok` | 15 | 25% | 31% |
| `missing_dep` | 17 | 28% | 35% |
| `parse_error` | 7 | 12% | 15% |
| `runtime_error` | 9 | 15% | 19% |
| `skip_guts` | 5 | 8% | — |
| `skip_native` | 6 | 10% | — |
| `no_provides` | 1 | 2% | — |

**Executed pure dists = 48** (60 − 5 guts − 6 native − 1 no-provides). Of the
**31** whose deps are satisfiable (48 − 17 missing_dep), **15 load (48%)** and
**16 hit a real mutsu bug (52%)**. Those 16 are the actionable frontier below.

> **⚠️ Re-sweep due (2026-07-19).** Since this table was generated, the
> `parse_error` bucket has been almost entirely cleared: #4833, #4842, #4845,
> #4847 (and intervening fixes) mean **every parse_error dist except CSS::Grammar
> now parses** (Taurus::CLI, Ecosystem::Archive, PDF::Combiner, Geo::Ellipsoid,
> Abbreviations, Business::CreditCard, Data::Dump). The counts above are stale;
> re-run the sweep to get fresh buckets before picking the next slice.

## ★ Actionable — real mutsu bugs (parse_error + runtime_error)

Grouped by root cause; a message hit by more than one dist is high-leverage.
**Before fixing:** reduce to a minimal repro from the dist source, fix generally,
add a `t/` pin, then re-check with `--only <Dist>`. (Same workflow as the mzef
pipeline.)

### Recurring (multiple dists — fix first)

| Root cause | Dists | Notes |
|---|---|---|
| ~~`Assignment operators inside ?? !! are too loose`~~ **FIXED (#4833)** | Web::App, RakuAST::Utils | A parenthesized accessor-lvalue assignment (`cond ?? a !! ($.value = x)`) was mis-rejected. **RakuAST::Utils now `load_ok`; Web::App advances to `missing_dep` (MIME::Types).** Pin: `t/ternary-paren-accessor-assign.t`. |
| `expected statement: expected expected statement…` (generic parse dead-end) | ~~**Geo::Ellipsoid**~~, **CSS::Grammar**, ~~**PDF::Combiner**~~, ~~**Ecosystem::Archive**~~, ~~**Taurus::CLI**~~ (all fixed but CSS::Grammar) | Was not one bug — each needed its own repro, and all but CSS::Grammar now parse. **Taurus::CLI** (#4842): `>>.{...}` hyper associative subscript did not parse, and hyper subscripts with a Range/list index (`>>.[0..2]`, `>>.{0..4}`) returned Nil instead of slicing each element (pin `t/hyper-assoc-subscript.t`). **Ecosystem::Archive** (#4842): a chained bind through an angle-word hash subscript (`my $id := %h<key> := VALUE`) failed because `<key>` was parsed as an expression, mis-reading `%h<b> := 5` as a `b > ...` comparison (pin `t/bind-angle-subscript-chain.t`). **PDF::Combiner** (#4847): a `$` anchor closing a substitution pattern (`s/'.pdf'$/.{$x}dpi/`) was mis-read as the `$/` match variable (pin `t/subst-pattern-dollar-anchor.t`). **Geo::Ellipsoid**: parses now (cleared by an intervening fix; a re-sweep is due to confirm its bucket). **Only CSS::Grammar remains** — it fails inside a `grammar` with "angle index key", so it is grammar/regex-slang, not a plain statement bug. **Also a cosmetic bug in the error itself**: the message doubles "expected expected" — worth fixing in the parse-error formatter. |

### Singletons

| Dist | Bucket | Root cause |
|---|---|---|
| Code::Coverable | runtime | `Unexpected block in infix position (missing statement control word…)` |
| Abbreviations | parse | ~~`Confused. Two terms in a row across lines`~~ **now parses** (cleared by an intervening fix; re-sweep due to confirm bucket). |
| JavaScript::Google::Charts | runtime | `Cannot declare individual multi candidates in 'our' scope` |
| Data::Dump | parse | ~~`Malformed double closure; WhateverCode is already a closure…`~~ **FIXED (#4836)** (`where { !$_.values.grep: * !~~ Sub }` — a `*`-arg in a where-block is not a double closure; pin `t/where-constraint-whatever-arg.t`). **Still blocked** by a second bug: `X::Syntax::Confused: Two terms in a row` (same class as Abbreviations). |
| Business::CreditCard | parse | ~~`X::Syntax::CannotMeta: Cannot negate * because it is not iffy enough`~~ **FIXED (#4845)** — a subset's `is` trait after `of BaseType` (`subset S of T is export where !*.…`) left the `where` stranded; `of`/`is` now parse in either order (pin `t/subset-trait-after-of.t`). Loads now; a separate latent regex char-class bug (`<-[\d x _ \  -]>` matching) only bites when a value is validated. |
| VERS | runtime | `X::Undeclared::Symbols: Undeclared routine` (load-time) |
| CSV-AutoClass | runtime | `Variable '$argstr' is not declared` (likely a signature/placeholder gap) |
| JSON::RepositoryEvent | runtime | `An exception occurred while evaluating a CHECK` |
| shorten-sub-commands | runtime | `Could not find as-cli-arguments` — self-provides / export resolution |

## missing_dep — reachable once deps are present (not mutsu bugs)

These fail only because a dependency dist is not on the path. Re-run with
`--site-repo` after `mzef install`-ing the dep to get a real verdict. The dep is
worth noting: a dep hit by many dists (e.g. **XML**, **Terminal::ANSI\***) is
itself a high-value bundle/compat target.

| Dist | Missing dep |
|---|---|
| deredere | HTTP::UserAgent |
| HTML::Parser | XML |
| File::Temp | File::Directory::Tree |
| DB::Xoos::MySQL | DB::MySQL |
| cro | File::Ignore |
| Qwiratry::Location::HTTP | HTTP::Tiny |
| Anolis | Terminal::ANSIParser |
| Version::Conan | Version::Semverish |
| FastCGI::NativeCall::Async | FastCGI::NativeCall |
| Chatnik | LLM::Functions |
| LLM::Prompts | XDG::BaseDirectory |
| Stomp | Concurrent::Iterator |
| Grammar::Message | Terminal::ANSIColor |
| XML::Query | XML |
| Net::Ethereum | Node::Ethereum::Keccak256::Native |
| Printing::Jdf | XML |
| Cro::HTTP::BodyParser::JSONClass | Cro::BodyParser |

**Repeated deps** (candidate bundle / compat targets): `XML` (×3), `Terminal::*`
(×2), `HTTP::Tiny` / `HTTP::UserAgent` (HTTP client — the known batteries gap).

## load_ok — proven to load on mutsu (n=60, seed 42)

App::FIT2GPX · App::SudokuHelper · Concurrent::Progress · CSV::Parser ·
File::Copy · File::Find · ForwardIterables · HexDump::Tiny · HTTP::HPACK ·
List::MoreUtils · List::Util · Modf · Staticish · Test::Builder · Test::Time

(Load-only; not a claim their test suites pass.)

## Planned deepenings

1. **Level 2 — run each dist's own test suite** (not just `use`): after
   `load_ok`, run `t/`/`.rakutest` under mutsu with deps installed. This catches
   runtime (not just load) divergence and is the truer "runs on mutsu" measure.
2. **Dep-closure sweep**: `mzef install` each sampled dist into a shared throwaway
   HOME so `missing_dep` collapses and second-order bugs (deps that themselves
   fail) surface.
3. **Widen the sample / track over time**: re-run at larger `--n`, record the
   bucket counts here per date to watch the load_ok rate climb as fixes land.
