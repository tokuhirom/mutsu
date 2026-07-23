# Battery: HTTP client

**Slot:** HTTP client · **Chosen (leaning):** `HTTP::UserAgent`
(`auth<zef:sergot>`, v1.2.0, MIT) · **Kind:** Adopted (community module, vendored
as-is) · **Sequenced after** the [TLS / HTTPS foundation](tls-openssl.md) ·
**Alternatives:** `HTTP::Tiny`, `Cro::HTTP::Client`, homegrown curl client

This record is the **template** for battery selection records (rationale +
alternatives + license). The client slot's decision is coupled to the TLS
decision, so read [tls-openssl.md](tls-openssl.md) alongside it.

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
- **Load timing caveat:** it declares `IO::Socket::SSL` as a hard dependency, so —
  unlike `HTTP::Tiny`'s lazy `require` — the first client battery is effectively
  **gated on the TLS foundation landing** (verify whether its `http://` path can
  run with `IO::Socket::SSL` merely installed-but-unused). This is the accepted
  cost of choosing it; it is why TLS is sequenced first.
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
