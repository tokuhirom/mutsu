# Battery: web framework — `Cro::HTTP` (+ `Cro::Core`, `Cro::TLS`)

**Slot:** Web framework · **Chosen:** `Cro::HTTP` v0.8.13 + `Cro::Core` v0.8.10
+ `Cro::TLS` v0.8.10 (all `auth<zef:cro>`, Artistic-2.0) · **Kind:** Adopted
(community modules, vendored as-is)

**Status: bundled and working.** The survey below (2026-07-31) picked Cro as
the target and set the campaign goal as "Cro::HTTP suite green under mutsu";
that goal was reached 2026-08-13 (Cro::HTTP 35/35 files, Cro::Core 9/9 —
see the "Bundling" section below), and Cro now ships as a battery.

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

## Bundling (2026-08-13): status: working

The campaign target above ("Cro::HTTP suite green under mutsu") was reached
after roughly 100 sessions of general-bug fixes to mutsu itself (cross-thread
`Supply`/`whenever`/closure semantics, method dispatch, typed lexicals,
regex/parser edge cases — none of it Cro-specific; see `news/2026-08/` and
`news/2026-07/` for the individual fixes). Final measurement:

| Suite | raku | mutsu (bundled) |
| --- | --- | --- |
| Cro::Core `t/` (9 files) | 9/9 | **9/9** |
| Cro::HTTP `t/` (35 files) | 35/35 | **35/35** |

All 44 files resolve and pass with **zero `-I` flags** — purely from the
bundled `modules/` tree (`Cro::HTTP`'s own test-local helper, `t/TestModule`,
still resolves via its own `use lib 't/TestModule'`, unaffected). This
matches the "zero-config `use`" bar every other battery meets.

**Cro::TLS is bundled alongside Cro::Core/Cro::HTTP** — it was not separately
surveyed above (the field enumeration was HTTP frameworks, and Cro::TLS is
Cro::HTTP's own declared TLS-transform dependency, not a competing choice)
but follows the same "adopt the real dependency" rule as the rest of the Cro
dependency chain (`crypt-random.md`, `io-path-childsecure.md`, `base64.md`,
`http-hpack.md`). Its own upstream suite (`t/types.rakutest`) is a single
file exercising `Cro::TLS::Configuration`; it passes and is registered in
`batteries.lock` like every other file here.

**New supporting dependencies this pulled in** (not previously bundled):
`Cro::Core`, `Cro::TLS`, `IO::Socket::Async::SSL`, `JSON::JWT`,
`Log::Timeline`, `CBOR::Simple`, `TinyFloats` — see
[cro-deps.md](cro-deps.md) for their individual records. Every other
`Cro::HTTP` dependency (`OO::Monitors`, `Crypt::Random`,
`IO::Path::ChildSecure`, `Base64`, `HTTP::HPACK`, `Digest::HMAC`,
`DateTime::Parse`, plus `JSON::Fast` — native) was already bundled ahead of
this campaign, per their own records.

### Why Cro::TLS was not a separate rejection/comparison exercise

Unlike the HTTP-*framework* choice (where Humming-Bird/Air/etc. were real
alternatives), there is no competing "TLS transform for Cro's pipeline" —
Cro::TLS is Cro's own module, required to run Cro::HTTP over `https://` or
HTTP/2 (`h2`/ALPN). The only decision was *whether to bundle Cro at all*,
already made above.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump a module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `Cro::Core` | <https://github.com/croservices/cro-core> | release-0.8.10 | `cfabfbc8` (2025-01-15) |
| `Cro::TLS` | <https://github.com/croservices/cro-tls> | release-0.8.10 | `2be4b0c1` (2025-01-15) |
| `Cro::HTTP` | <https://github.com/croservices/cro-http> | release-0.8.13 | `6238e753` (2026-06-02) |

```sh
# 1. Clone the new upstream revision, then copy the runtime tree + attribution.
#    Upstream tests/CI/precomp/dev-utility (t/, it/, utils/, dist.ini) are
#    deliberately NOT vendored: the release gate fetches the tests fresh at
#    the pinned commit (BATTERIES.md §3).
rsync -a --exclude '.precomp' <checkout>/lib/ modules/Cro-HTTP/lib/
cp <checkout>/{META6.json,LICENSE,README.md,Changes} modules/Cro-HTTP/

# 2. Bump the module's `commit` row in batteries.lock and the table above
#    (three rows: Cro::Core, Cro::TLS, Cro::HTTP — bump together, they are
#    released as one project and version-locked against each other via
#    META6.json `depends` ranges).
# 3. Re-run the gate and review the diff (a newly failing file is a
#    regression to fix, not to whitelist away):
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt

# 4. Refresh the Pages manifest:
python3 scripts/gen-batteries-manifest.py
```

## API sketch

```raku
use Cro::HTTP::Router;
use Cro::HTTP::Server;

my $application = route {
    get -> 'hello', $name {
        content 'text/plain', "Hello, $name!";
    }
}

my Cro::Service $service = Cro::HTTP::Server.new(:host<0.0.0.0>, :port(10000), :$application);
$service.start;
react whenever signal(SIGINT) { $service.stop; exit; }
```

`Cro::TLS` layers HTTPS/HTTP2 on top, given a cert/key pair:

```raku
use Cro::HTTP::Server;
use Cro::TLS;

my $service = Cro::HTTP::Server.new(
    :host<0.0.0.0>, :port(10443), :$application,
    tls => %( private-key-file => 'key.pem', certificate-file => 'cert.pem' ),
);
```

## License

**Artistic-2.0** — declared in each dist's `META6.json` and shipped as
`LICENSE`. Vendored verbatim with `LICENSE` / `META6.json` / `README` /
`Changes` preserved for attribution, source unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).

## Reproduction (of the original survey)

Survey harness (fetch + closure of `-I` paths + per-file prove) lives in the
session scratchpad, pattern identical to `tmp/tmpl-survey.sh` from the
template-slot survey; re-derive from [selection-method.md](selection-method.md)
§3. Key detail for Cro::HTTP: `zef install --/test JSON::Fast OO::Monitors
Base64 HTTP::HPACK IO::Path::ChildSecure JSON::JWT DateTime::Parse
Crypt::Random Log::Timeline IO::Socket::Async::SSL Cro::Core Cro::TLS` first,
then run each `t/*.t` with only `-I <dist>/lib`.
