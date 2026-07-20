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

The headline finding: among the pure-Raku dists whose deps are satisfiable,
**57%** load cleanly (n=150/seed7, up from 48% at n=60/seed42); the rest hit a
genuine mutsu parse/runtime bug on `use` alone. So real-dist compatibility, not
roast, is the productive frontier for widening the batteries base (PLAN.md §1 B4).
The list of dists proven to load is the [load_ok section](#load_ok--proven-to-load-on-mutsu-n150-seed-7).

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
MUTSU_BIN=target/release/mutsu scripts/dist-compat-sweep.py --n 150 --seed 7 --timeout 20
# → tmp/dist-compat-sweep.tsv (per provided-module rows) + a bucket summary +
#   the load_ok dist list (the passing-dist report) printed at the end
```

The script prints, after the bucket summary, the full `load_ok` dist list so the
passing-dist report below can be refreshed in one command. **Rebuild the release
binary first** (`touch src/main.rs && cargo build --release`) — the sweep silently
grabs a stale binary otherwise.

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

## Latest run — n=150, seed 7 (2026-07-19, sandboxed via bwrap, main + #4865)

| Bucket | Count | % of 150 | % of executed pure (121) |
|---|---|---|---|
| `load_ok` | 44 | 29% | 36% |
| `missing_dep` | 44 | 29% | 36% |
| `parse_error` | 18 | 12% | 15% |
| `runtime_error` | 15 | 10% | 12% |
| `skip_native` | 22 | 15% | — |
| `skip_guts` | 7 | 5% | — |

**Executed pure dists = 121** (150 − 7 guts − 22 native). Of the **77** whose
deps are satisfiable (121 − 44 missing_dep), **44 load (57%)** and **33 hit a
real mutsu bug (43%)** — 18 parse, 15 runtime. Those 33 are the actionable
frontier below. (The load-rate among satisfiable pure dists has climbed from
48% at n=60/seed42 to 57% here as parse/type fixes land.)

The full passing (`load_ok`) list is in the [load_ok section](#load_ok--proven-to-load-on-mutsu-n150-seed-7)
below — this is the "which real dists run on mutsu today" report. Regenerate
both tables together with `scripts/dist-compat-sweep.py` (it now prints the
`load_ok` dist list at the end of a run).

## ★ Actionable — real mutsu bugs (parse_error + runtime_error)

Grouped by root cause; a message hit by more than one dist is high-leverage.
**Before fixing:** reduce to a minimal repro from the dist source, fix generally,
add a `t/` pin, then re-check with `--only <Dist>`. (Same workflow as the mzef
pipeline.)

The 33 actionable dists in the n=150/seed7 sample (18 parse, 15 runtime). The
generic `expected statement: expected expected statement…` parse dead-end is
**not one bug** — each dist needs its own minimal repro (past slices #4842,
#4845, #4847, #4858, #4865 each cleared a different construct behind that same
message). Work them one at a time.

### parse_error (18)

App::Rak · Astro::Utils · Audio::Liquidsoap · Configuration ·
Cro::FCGI · CSS · CSV::Table · Deps · File-TreeBuilder · IP::Random ·
Math::Matrix · ML::SparseMatrixRecommender · Pod::Contents ·
SBOM::CycloneDX · SSH::LibSSH::Tunnel

Known root causes to date:

| Dist | Root cause |
|---|---|
| SSH::LibSSH::Tunnel | `Cannot have a 'whenever' block outside the scope of a 'supply' or 'react' block` — a `whenever` reached at load time (likely a react/supply parse gap). |
| SBOM::CycloneDX | `unparsed input, column 5: "}\n… @*ERRORS"` — the reported line (the `}` closing `method ingest`) is the last-good boundary, **not** the cause: the `@*ERRORS`/`with…else`/private-method-call constructs there all parse in isolation. The real desync is a still-unisolated construct in the tail `Map`/`Hash` methods (deep `mapify` ternary, `my role ordered-list[@NAMES]`, `$map but ordered-list[@keys]`) that forces the whole-role parse to backtrack. Investigating it surfaced a *separate* general bug — interpolated `.join(", ")` (a method arg carrying the string's own quote/comma) was mis-split, fixed separately — but that was not the SBOM blocker. Needs a finer tail-method repro. |
| CSS | inside a `grammar` ("angle index key") — grammar/regex-slang, heavier. |
| ~~Bench~~ | **Cleared post-snapshot**: a tightly-bound `<=>` used as a quote-word / hash key / colonpair value (`:fill<=>`, `%h<=>`) was mis-parsed as the spaceship operator by three separate angle parsers. Now loads. |
| Pod::Contents / Deps / Configuration / File-TreeBuilder / App::Rak / Astro::Utils / Audio::Liquidsoap / IP::Random / Math::Matrix / ML::SparseMatrixRecommender / CSV::Table / Cro::FCGI | generic `expected statement…` dead-end — each needs its own repro; several carry multiple blockers (bisect at balanced method boundaries). |
| ~~Trie~~ | **Cleared post-snapshot**: was a set-operator compound assignment on an indexed lvalue (`%!decendents{char} ∪= …`) the parser rejected. Parse error gone; now only lacks its `OrderedHash` dependency (missing_dep). |
| ~~Prime::Factor~~ | **Cleared post-snapshot**: was a sigilless parameter (`\N`) carrying a `where` constraint (`multi divisors (Int \N where BIG, …)`) that the param parser rejected. Now loads and factors correctly. |

### runtime_error (12 remaining; Astro::Sunrise / JavaScript::Google::Charts / App::pixelpick cleared post-snapshot)

| Dist | Root cause |
|---|---|
| Protocol::MQTT | `Invalid typename 'DecodeBuffer' in parameter declaration.` — a sibling type in the enclosing package (same family as #4865; `DecodeBuffer` is likely nested deeper / declared via a form the prefix-walk still misses). |
| SQL::Abstract | `No matching candidate found for the parametric role` — advanced past four typename blockers by #4865; now blocked on parametric-role resolution in its `does Constant['…']` / `does Op::Prefix['…']` chains. |
| PDF::Font::Loader::CSS | `X::Syntax::Perl5Var: Unsupported use of $? variable` — a genuine `$?`-in-regex Perl5-ism (verify against raku before "fixing"). |
| uniname-words | `Odd number of elements found where hash initializer expected` (load-time) — an `nqp::hash(...)` in a `BEGIN` block; guts-bound, not pursued. |
| Repository::Precomp::Cleanup | `No such method 'id' for invocant of type 'Compiler'`. |
| Test::Scheduler | `Scheduler is not composable, so Test::Scheduler cannot compose it`. |
| Bits | `An exception occurred while evaluating a CHECK` (load-time CHECK phaser). |
| RakudoContainerfileBuilder | prints a `Usage:` block at load — a MAIN/`is export` interaction. |
| App::fix.raku / Lingua::NumericWordForms | `self-module not found` — packaging/name-mismatch (provided module name ≠ what `use` resolves). |
| Testo | non-zero exit with no diagnostic captured — needs a direct run to classify. |

**Cleared post-snapshot:**

- IDNA::Punycode (was `Unexpected block in infix position` — a statement label
  on a C-style `loop (init; cond; step)` that the labeled-loop parser did not
  handle) now loads; its `decode_punycode` still hits a separate
  list-assignment runtime bug, a Level-2 concern.
- JavaScript::Google::Charts (was `Cannot declare individual multi candidates in
  'our' scope` — mutsu hoisted the `our multi` candidates before the in-sequence
  `our proto` registered, so the scope check fired against an empty proto table;
  fixed in #4895) now advances to `missing_dep` (`Data::TypeSystem`).
- App::pixelpick (was `X::Undeclared::Symbols: Undeclared routine: qq:to` — a
  `say qq:to` heredoc whose quoted delimiter sits on the next line; mutsu's
  delimiter scanner trimmed only spaces, not the newline) now advances to
  `missing_dep` (`Color::Names`).

## missing_dep — reachable once deps are present (not mutsu bugs)

These fail only because a dependency dist is not on the path. Re-run with
`--site-repo` after `mzef install`-ing the dep to get a real verdict. The dep is
worth noting: a dep hit by many dists (e.g. **XML**, **Terminal::ANSI\***) is
itself a high-value bundle/compat target.

The 44 missing_dep dists in the n=150/seed7 sample and the dep each first
needs (via `awk -F'\t' '$3=="missing_dep"' tmp/dist-compat-sweep.tsv`):

| Dep needed | Dependent dists |
|---|---|
| `LLM::DWIM` | SHAI, WAT--CLI |
| `Date::Calendar::Strftime` | Date::Calendar::Bahai, Date::Calendar::MayaAztec |
| `Geo::Geometry` | Geo::WellKnownBinary, Geo::WellKnownText |
| `HTTP::UserAgent` | Geo::Coder::OpenCage |
| `HTTP::Tiny` | WWW::MistralAI |
| `HTTP::Status` | HTTP::Roles |
| `Cro::HTTP::Router` | Cro::WebApp::Evaluate |
| `XML::Fast → LibXML`, `Text::CSV`, `Digest::MD5`, `Digest::SHA1::Native`, `String::CRC32` | XML::Fast, Services::PortMapping, Trove, DB::Migration::Declare, Hash::Consistent |
| (many DSL::* / ML::* / LLM::* / Intl::* internal deps) | DSL::English::DataQueryWorkflows, ML::AssociationRuleLearning, MCP::Client, Intl::CLDR, … |

**Repeated deps** (candidate bundle / compat targets): `LLM::DWIM` (×2),
`Date::Calendar::Strftime` (×2), `Geo::Geometry` (×2), and the HTTP-client
family (`HTTP::Tiny` / `HTTP::UserAgent` / `HTTP::Status` — the known batteries
gap). An HTTP client and the `Date::Calendar::*` / `Geo::*` roots each unblock a
cluster at once.

## load_ok — proven to load on mutsu (n=150, seed 7)

The passing-dist report: every sampled distribution whose provided modules all
`use` cleanly on mutsu (main + #4865, plus post-snapshot fixes). **44 of 150**
in the raw seed-7 run (57% of the 77 whose deps are satisfiable); Astro::Sunrise
below was cleared after the snapshot by the `unit class … is export;` trait fix.

Algorithm::LCS · Ask · Astro::Sunrise · Async::Command · Attribute::Predicate ·
Automata::Cellular · bird · CheckSocket · CLDR::List · CodeUnit · CSV::Parser ·
Deepgrep · English · File::Ignore · Game::Covid19 · Grid · Hash::Agnostic ·
Hash::Restricted · HTTP::HPACK · IdClass · Math::Interval · Math::Random ·
Metropolis · Modf · Moonphase · Net::Whois · P5opendir · P5push · P5quotemeta ·
Point · Raku::Elements · RakupodObject · Rat::Precise · RPi::Device::DS18B20 ·
RSV · Scalar::Util · SnowFlake · Subset::Helper · Terminal::ANSIParser ·
Text::Tabs · Time::Duration · Time::Duration::Parser · Trap · Util::Uuencode ·
ValueList

(Load-only; not a claim their test suites pass — see Level 2 below.)

## Planned deepenings

1. **Level 2 — run each dist's own test suite** (not just `use`): after
   `load_ok`, run `t/`/`.rakutest` under mutsu with deps installed. This catches
   runtime (not just load) divergence and is the truer "runs on mutsu" measure.
2. **Dep-closure sweep**: `mzef install` each sampled dist into a shared throwaway
   HOME so `missing_dep` collapses and second-order bugs (deps that themselves
   fail) surface.
3. **Widen the sample / track over time**: re-run at larger `--n`, record the
   bucket counts here per date to watch the load_ok rate climb as fixes land.
