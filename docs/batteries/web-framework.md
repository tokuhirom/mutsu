# Battery survey: web framework

**Status: surveyed, no winner bundled yet — the survey's output is a work list
(as predicted by [selection-method.md](selection-method.md) §5).**

Date: 2026-07-31. Method: [selection-method.md](selection-method.md) — field
enumerated from the fez index (7711 entries; recency taken from the CDN
tarball `Last-Modified`, since the fez index carries no dates), licenses and
reverse-dependency counts from the same index, then every candidate's own
upstream test suite run under `raku` (2026.06) first and mutsu (release)
second, from the dist's own directory, one `prove` per file.

This is the slot the batteries yardstick points at directly: *"a small web
blog can be written with the bundle alone"* (BATTERIES.md §2).

## The field

Frameworks registered on fez with a release in the last ~3 years:

| Candidate | Ver | Last release | License | Runtime deps character | Dependents |
| --- | --- | --- | --- | --- | --- |
| **Cro::HTTP** (+ Core/TLS/WebApp) | 0.8.9.1 | 2024-01 (WebApp 2025-01) | Artistic-2.0 | OO::Monitors, IO::Socket::Async::SSL→OpenSSL, 9 more | **61** |
| **Humming-Bird** | 4.1.0 | 2026-07 | MIT | 9 pure-Raku dists (JSON::Fast, HTTP::Status, MIME::Types, …); Cro::HTTP::Client only in integration tests + one APM plugin | 1 |
| **Air** | 0.1.9 | 2026-02 | Artistic-2.0 | hard-depends on Cro::HTTP + Cro::WebApp + Cromponent | 9 (own plugins) |
| **MVC::Keayl** | 0.9.1 | 2026-07 | Artistic-2.0 | Cro::HTTP + OpenSSL + Digest::SHA1::Native + own ORM/HAML | 1 |
| **Web::App** | 0.9.2 | 2026-01 | Artistic-2.0 | PSGI, SCGI, FastCGI, HTTP::Easy, MIME::Types (all pure) | 1 |
| **Router::Right** (router only) | 0.0.61 | 2024-08 | Artistic-2.0 | P5quotemeta | 1 |
| Crolite / Cromponent | 0.0.1 / 0.0.9 | 2026-01 / 2025-01 | Artistic-2.0 | Cro-stack accessories | 0 / 1 |

Out of scope: Hiker (2021), Web::App::MVC (2022) — no release in 3 years;
Bailador, Hematite — not on fez. `HTTP::Easy` (2026-01, PSGI server) is
Web::App's engine, surveyed with it.

Ecosystem standing is lopsided: **Cro::HTTP has 61 dependents** — it is the de
facto standard, and Air/MVC::Keayl/Cromponent/Crolite are all built on it.
Humming-Bird is the only actively-maintained framework that is pure Raku all
the way down.

## Measured suite results (2026-07-31)

`raku` column measured first per the method; a file counts as passing only
when prove accepts its TAP. mutsu = release build, `timeout 60` per test.

| Suite | raku | mutsu | Dominant mutsu blocker |
| --- | --- | --- | --- |
| Humming-Bird t/ (14 files) | 10/14 (a) | 5/14 | one parser bug, see below |
| Router::Right (3) | 3/3 | 0/3 | real subtest failures (uninitialized-Any warnings; unreduced) |
| Web::App (1) | 1/1 | 1/1 | — |
| HTTP::Easy (1) | 1/1 | 1/1 | — |
| OO::Monitors (5) | 5/5 | 0/5 | `monitor` declarator (EXPORTHOW::DECLARE) unimplemented |
| Cro::Core (9) | 9/9 | 1/9 | `Cro::Service.start` missing, `.message` on List, 3 subtest-level failures |
| Cro::HTTP (28) | **28/28** (b) | 1/28 | the two tickets below gate nearly everything |

(a) 4 files (`t/07`–`t/10`) fail under raku itself with a duplicate-import
collision (`use Humming-Bird::Core` + `use Humming-Bird::Glue`) — dead
upstream, not a mutsu target. So the reachable HB baseline is 10.
(b) With its deps properly `zef install`ed. Running deps from `-I` source
trees breaks `OpenSSL::NativeLib`'s `%?RESOURCES` handling under raku too —
survey with installed deps or you will under-measure the baseline (this
survey's first pass read 5/28 for exactly that reason).

### The two bugs that gate most of the field

1. **[`todo/tickets/brace-subscript-after-call-and-parens.md`](../../todo/tickets/brace-subscript-after-call-and-parens.md)**
   — `routes{'/'}` / `($a // $b){$key}` postcircumfix `{}` mis-parsed as a
   block/hash argument. Gates 5 of Humming-Bird's 6 mutsu-attributable
   failures **and** `use Cro::HTTP::Router` (parse error at Router.pm6:188).
   One fix, two frameworks.
2. **[`todo/tickets/cro-bodyserializers-required-method-false-positive.md`](../../todo/tickets/cro-bodyserializers-required-method-false-positive.md)**
   — false "Method 'serialize' must be implemented" on a proto/multi
   implementation; blocks `Cro::HTTP::Request`/`Response` load.

## Cro reachability — the 2026-07 re-assessment

Earlier project notes ("Cro = nqp/CStruct territory", 2026-06 ecosystem
scouting; "IO::Socket::Async::SSL … which mutsu has no foundation for",
[http-client.md](http-client.md)) are **stale**. Measured against the current
releases:

- **No `use nqp` anywhere** in Cro::Core / Cro::HTTP / Cro::TLS / OO::Monitors
  / Log::Timeline / HTTP::HPACK. The only `nqp::` in the whole chain is one
  `nqp::sha1` line in `OpenSSL::NativeLib` (a resources-path helper).
  *Correction (same day, found by load-probing after the brace-subscript fix):*
  that claim covers the 7 dists' own code, but Log::Timeline **eagerly loads**
  its `CBOR::Simple` output backend, which is an `nqp_ops_only` dist (buffer
  read/write ops). That makes the nqp-op subset a real, bounded prerequisite
  for loading Cro::HTTP — see
  [`todo/tickets/cbor-simple-nqp-buf-ops.md`](../../todo/tickets/cbor-simple-nqp-buf-ops.md).
- **NativeCall surfaces are small and known**: the OpenSSL binding (~94
  `is native` functions — and mutsu already bundles and drives `OpenSSL` for
  the sync TLS battery) and one `setsockopt` in `Cro::Core`'s TCP_NODELAY.
- **`monitor` (OO::Monitors) is needed by only 3 Cro::HTTP files** — Client,
  CookieJar, Session::InMemory. The server-side core does not use it. It is
  the one genuine `deep_guts` item (EXPORTHOW::DECLARE + a MetamodelX
  metaclass), and would need the "narrow per-feature shim" the
  [guts survey](../ecosystem-guts-dependency-survey.md) recommends —
  a built-in `monitor` declarator, not a general slang layer.
- Probes on today's mutsu: `Cro::Message` loads and composes as a role;
  `Cro::TCP`, `Cro::Uri`, `Cro::MediaType`, `Cro::HTTP::BodyParsers` all
  `use` cleanly. What remains is ordinary pure-Raku compatibility work of the
  kind every battery campaign has consisted of — plus, for the async server
  path, real `IO::Socket::Async` + supply-pipeline load (mutsu's S17 layer
  gets its first production-shaped workout).

## Leaning: Cro first (decision updated 2026-07-31, same day)

**Original leaning was "Humming-Bird short-term, Cro mid-term". The user
overruled it the same day, and the survey data supports the reversal**: 4 of
Humming-Bird's 14 test files fail under current raku itself (a duplicate
double-import that a maintained project would have caught), it has 1
dependent, and its parser blocker turned out to be shared with Cro anyway.
A battery whose upstream does not keep itself green on raku is a weak
foundation to bundle.

- **The slot's target is Cro** — the same role zef plays for the module
  pipeline: 61 dependents, raku baseline 28/28, and Air (the most
  interesting modern DX, HTMX-oriented, 2026-02) plus the rest of the Cro
  ring become reachable if and only if Cro::HTTP runs. Do not bundle Cro
  yet; treat "Cro::HTTP suite green under mutsu" as the campaign target
  measured by this survey's harness. Known gate order after the brace-
  subscript (#5599) and role-stub (name-based satisfaction) fixes:
  `CBOR::Simple`'s nqp ops (gates `Log::Timeline`, and therefore
  `use Cro::HTTP::Router` — the load-time slice is small: `bitor_i`,
  `nqp::const::BINARY_*`, `Encoding::Registry`; the buf read/write family
  only runs when log outputs are activated), then `Cro::Service.start`,
  then a built-in `monitor` for the client/session files.
- **Humming-Bird stays a measured compat data point** (10/14 = its raku
  baseline as of the two fixes above), not the battery. Its remaining
  failures (t/04 middleware assignment, t/13 live-server flow) are still
  worth fixing as general bugs when convenient.
- **Rejected for the slot**: Web::App/HTTP::Easy (healthy and tiny, but a
  synchronous PSGI/SCGI/FastCGI design from the pre-async era with 1
  dependent; its suites already pass — keep as a compat data point, not a
  battery); MVC::Keayl (Cro-gated *and* single-author stack with its own ORM);
  Air/Cromponent/Crolite (Cro-gated — revisit after Cro); Router::Right
  (not a framework; its 0/3 is a real mutsu bug cluster worth fixing but the
  dist doesn't fill the slot).

## Reproduction

Survey harness (fetch + closure of `-I` paths + per-file prove) lives in the
session scratchpad, pattern identical to `tmp/tmpl-survey.sh` from the
template-slot survey; re-derive from [selection-method.md](selection-method.md)
§3. Key detail for Cro::HTTP: `zef install --/test JSON::Fast OO::Monitors
Base64 HTTP::HPACK IO::Path::ChildSecure JSON::JWT DateTime::Parse
Crypt::Random Log::Timeline IO::Socket::Async::SSL Cro::Core Cro::TLS` first,
then run each `t/*.t` with only `-I <dist>/lib`.
