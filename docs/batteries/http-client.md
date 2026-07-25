# Battery: HTTP client

**Slot:** HTTP client · **Chosen:** `HTTP::UserAgent` (`auth<zef:sergot>`,
v1.2.0, MIT) · **Kind:** Adopted (community module, vendored as-is) ·
**Sequenced after** the [TLS / HTTPS foundation](tls-openssl.md) ·
**Alternatives:** `HTTP::Tiny`, `Cro::HTTP::Client`, homegrown curl client

This record is the **template** for battery selection records (rationale +
alternatives + license). The client slot's decision is coupled to the TLS
decision, so read [tls-openssl.md](tls-openssl.md) alongside it.

## Status: bundled and working

`HTTP::UserAgent` ships at `modules/HTTP-UserAgent/` and resolves with **zero
config** — `use HTTP::UserAgent;` with no `-I` and no install:

```raku
use HTTP::UserAgent;
my $res = HTTP::UserAgent.new.get('https://example.com/');
say $res.code;          # 200 — real TLS, via the bundled OpenSSL stack
```

Its **whole upstream test suite passes: 27/27 files**, unmodified, against the
bundled copy. Nothing was patched into the vendored sources; every gap the suite
exposed was closed in the interpreter (see the `news/2026-07/` entries from the
HTTP::UserAgent campaign).

23 of those 27 files are in
[`batteries-whitelist.txt`](../../batteries-whitelist.txt), so the release-time
gate re-runs them against the shipped library on every release. The four that are
not fall into two groups:

- **`110-redirect-cookies`, `230-binary-request`, `250-issue-144`** need
  `Test::Util::ServerPort`, a **test-only** dependency (`META6.json`
  `test-depends`) that is deliberately not bundled and that
  `scripts/battery-testsuite.sh` has no way to fetch — it clones only the
  battery's own repository. They fail in the gate with `ok=0` because the module
  is missing, not because an assertion fails. They pass locally with it on the
  path.
- **`082-exceptions`** is in
  [`batteries-exclude.txt`](../../batteries-exclude.txt): it makes unguarded live
  requests to `httpbin.org`, so its verdict depends on a third-party service and
  it must not be able to block a release. (`httpbin.org` spent part of
  2026-07-25 returning 503, while this battery was being bundled.) The gate skips
  it entirely.

The rest of the suite's live-network assertions are guarded by the file's own
`NETWORK_TESTING` check, which the gate does not set, so they are deterministic
there. That was measured rather than assumed — every whitelisted file was re-run
inside a loopback-only network namespace and only `082-exceptions` failed; see
[testsuite-gate.md](testsuite-gate.md).

## Decision and sequencing

The guiding yardstick is **"a small web blog can be written with the bundle
alone."** For that, the client needs real HTTPS, which every pure-Raku client
delegates to `IO::Socket::SSL` → `OpenSSL`. So the client slot is **built bottom-up**:

1. **TLS foundation first** — bundle `OpenSSL` + `IO::Socket::SSL` and grow mutsu's
   NativeCall to run them ([tls-openssl.md](tls-openssl.md)). This is the active
   first target.
2. **Then the client** — adopt the mature **`HTTP::UserAgent`**.

**Why `HTTP::UserAgent` over the lighter `HTTP::Tiny`:** the usual argument for
`HTTP::Tiny` is its zero dependencies. But **once we commit to bundling the TLS
stack, dependency-weight stops being the deciding axis** — `HTTP::UserAgent`'s
extra dependencies are all pure-Raku (`HTTP::Status`, `URI`, `Encode`,
`DateTime::Parse`, plus already-working `File::Temp` / `MIME::Base64`), i.e. the
"grow-the-core" surface we want to exercise anyway. In exchange we get the
**classic, full-featured de-facto client** (cookies, sessions, the `HTTP::Message`
object family) that makes the blog use-case ergonomic. (User decision, 2026-07-24.)

**`HTTP::Tiny` is kept as a documented alternative / possible early win** — it is
zero-dep, verified to load on mutsu today, and lazily `require`s TLS, so it *could*
land an `http://`-only battery before the TLS foundation is ready if we want an
earlier milestone. It is not the primary choice.

## Candidates

The Raku ecosystem has **no single crowned HTTP client** (unlike Python's
`requests`); the field splits three ways, plus a homegrown option.

### ✅ `HTTP::UserAgent` (`zef:sergot`, v1.2.0, MIT) — chosen

- **What it is:** the long-standing, mature general-purpose blocking client,
  loosely modeled on Perl's `LWP::UserAgent`. Rich feature set (cookies, sessions,
  redirects, the `HTTP::Message` family).
- **Dependencies:** `HTTP::Status`, `DateTime::Parse`, `Encode`, `URI`,
  `File::Temp`, `MIME::Base64`, and `IO::Socket::SSL` (→ `OpenSSL`). All
  permissive; the non-TLS ones are pure Raku.
- **Load timing caveat (resolved):** it declares `IO::Socket::SSL` as a hard
  dependency, so — unlike `HTTP::Tiny`'s lazy `require` — the client battery was
  **gated on the TLS foundation landing**. That is why TLS was sequenced first;
  with the TLS stack bundled, this cost has been paid and both `http://` and
  `https://` work.
- **License:** MIT.

### `HTTP::Tiny` (`zef:jjatria`, v0.2.6, Artistic-2.0) — alternative / early win

- **What it is:** "a small, simple, correct HTTP/1.1 client," a Raku port of Perl's
  `HTTP::Tiny`. Response is a hash: `<success>/<status>/<reason>/<content>/<headers>`.
- **Strengths:** `"depends": []` (**zero** deps, single file `lib/HTTP/Tiny.rakumod`),
  and it **loads + constructs on mutsu today** (`use HTTP::Tiny; HTTP::Tiny.new`
  works; `.^name` resolves). HTTPS is a lazy `try require ::('IO::Socket::SSL')`, so
  `http://` works without the TLS stack present.
- **Why not primary:** its one advantage (zero deps) is neutralized once we bundle
  the TLS stack for `HTTP::UserAgent` anyway, and it is more minimal (no cookie jar,
  no `HTTP::Message` family). Retained as the fallback / earlier-milestone option.
- **Known mutsu gap (if used):** a real request currently fails with
  `Unknown function: split-url` (surfaced as HTTP status 599). `split-url` is a
  lexical `sub` in the `HTTP::Tiny` class body (`lib/HTTP/Tiny.rakumod:380`) called
  from private `method !request`. Reduced repros (class-body lexical sub via
  `proto`/`multi`/private method, inside `CATCH`, from a forward-declared nested
  class, through a redispatch chain) **all pass individually**, so it is an
  interaction bug specific to the full module needing dedicated isolation — a
  general lexical-scope/dispatch fix, not an `HTTP::Tiny` hack.
- **License:** Artistic-2.0.

### `Cro::HTTP::Client` (`zef:cro`) — rejected

Part of **Cro**, the prominent modern Raku stack for reactive/async network
services (HTTP client + server, WebSockets, TLS, routing) on the
`supply`/`react`/`whenever` model. Rejected as a first battery: async-first, pulls
in the whole Cro stack, and its TLS relies on `IO::Socket::Async::SSL` (async +
OpenSSL) which mutsu has no foundation for. Overkill for "make one request."

### Homegrown curl-shellout client — rejected

A mutsu-specific client driving system `curl` via `Proc::Async` (the pattern the
bundled Zef uses to fetch). Rejected: it is a **rung-3 last resort** (private
reimplementation) the policy avoids when a community option is viable, and it is an
**architectural mismatch** — `HTTP::UserAgent` / `HTTP::Tiny` speak HTTP/1.1
themselves over a *socket* abstraction, whereas `curl` speaks the whole protocol,
so `curl` cannot be slotted in as the socket/TLS backend those clients drive.

## TLS / HTTPS

Delegated to the shared foundation — see **[tls-openssl.md](tls-openssl.md)** for
the full analysis: why `IO::Socket::SSL` → `OpenSSL` is the common layer, the
concrete NativeCall gaps mutsu must grow to run it, the packaging impact
(`libssl3` in the Docker runtime stage), and the security-update story
(TLS CVEs ride the OS).

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `HTTP::UserAgent` | <https://github.com/raku-community-modules/HTTP-UserAgent> | v1.2.0 | `1d6a31a0` (2025-05-04) |

What is vendored: `lib/` plus `META6.json`, `LICENSE`, `README.md`, `Changes`
for attribution. Upstream `t/`, `xt/`, `doc/`, `examples/`, `dist.ini`, CI config
and `.precomp` artifacts are deliberately excluded — the release gate fetches the
tests fresh at the pinned commit (BATTERIES.md §3).

```sh
# 1. Clone the new upstream revision, then copy the runtime tree + attribution.
rsync -a --exclude '.precomp' <checkout>/lib/ modules/HTTP-UserAgent/lib/
cp <checkout>/{META6.json,LICENSE,README.md,Changes} modules/HTTP-UserAgent/

# 2. Bump the `commit` in batteries.lock and the table above.
# 3. Re-run the gate and review the diff (a newly failing file is a regression
#    to fix, not to whitelist away):
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt

# 4. Refresh the Pages manifest:
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump — the zero-config smoke test:

```sh
mutsu -e 'use HTTP::UserAgent; say HTTP::UserAgent.new.get("https://example.com/").code'   # 200
```

A *deployed* mutsu can also take a patched module without a re-vendor —
`mzef install HTTP::UserAgent` shadows the bundled copy. Re-vendoring is for the
next release, so fresh installs ship the fix too.

## Security updates

Per [BATTERIES.md §6](../../BATTERIES.md#6-security-updates-and-independent-updatability):
the bundled client is the lowest-priority source, so a patched version installed
with `mzef install HTTP::UserAgent` shadows the bundled copy without a mutsu
release. Its TLS security rides the system `libssl` (see tls-openssl.md).

## License

- **`HTTP::UserAgent`** — MIT. Upstream: <https://github.com/raku-community-modules/HTTP-UserAgent>.
- **`HTTP::Tiny`** (alternative) — Artistic-2.0. Upstream:
  <https://gitlab.com/jjatria/http-tiny>, author José Joaquín Atria.
- Whichever is bundled is vendored verbatim with its `LICENSE` / `META6.json` /
  `README` preserved for attribution; source unmodified (per
  [BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
