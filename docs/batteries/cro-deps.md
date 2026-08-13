# Battery: Cro's new supporting dependencies — `TinyFloats`, `CBOR::Simple`, `IO::Socket::Async::SSL`, `JSON::JWT`, `Log::Timeline`

**Slot:** Cro dependency layer (the new pieces, not already bundled) · **Kind:**
Adopted (community modules, vendored as-is) · **Licenses:** Artistic-2.0
(TinyFloats, CBOR::Simple, IO::Socket::Async::SSL, Log::Timeline) / MIT
(JSON::JWT)

These are `Cro::HTTP`'s declared `depends` that were **not** already bundled
ahead of the [Cro battery](cro-http.md) — the other dependencies
(`OO::Monitors`, `Crypt::Random`, `IO::Path::ChildSecure`, `Base64`,
`HTTP::HPACK`, `Digest::HMAC`, `Digest`, `DateTime::Parse`, plus `JSON::Fast`
— native) each already have their own record. Like those, they are bundled
as a layer because none is useful to sequence separately: `Cro::HTTP` cannot
ship until all of them do.

## What they are

| Module | Purpose | Cro::HTTP's use |
| --- | --- | --- |
| `TinyFloats` | Half/bfloat16/tf32/fp8 float codec | `CBOR::Simple`'s dependency (CBOR's minor-type-7 float encodings) |
| `CBOR::Simple` | CBOR (RFC 8949) codec | `Log::Timeline`'s CBOR-sequence output backend |
| `IO::Socket::Async::SSL` | Async TLS sockets (`IO::Socket::Async`-shaped API) | `Cro::TLS`'s transport |
| `JSON::JWT` | JSON Web Token encode/decode/verify | `Cro::HTTP::Auth::WebToken` (bearer-token auth) |
| `Log::Timeline` | Structured start/end/event logging | Cro's request/response timeline instrumentation |

## Status

| Module | Own upstream suite | Result | Sufficient for Cro::HTTP? |
| --- | --- | --- | --- |
| `TinyFloats` | 5 files | **5/5** | yes |
| `JSON::JWT` | 2 files | **2/2** | yes |
| `CBOR::Simple` | 7 files | 1/7 (`00-use`) | yes — see below |
| `IO::Socket::Async::SSL` | 8 files | 0/8 | yes — see below |
| `Log::Timeline` | 5 files | 1/5 (`has-output`) | yes — see below |

`TinyFloats` and `JSON::JWT` fully pass their own upstream suites and are
registered whole in `batteries.lock`/`batteries-whitelist.txt`. The other
three are bundled and **sufficient for what Cro::HTTP actually needs** — its
own 35/35-passing suite proves that — but each has a real, narrower gap in
its *own* broader upstream suite, tracked as follow-up work rather than
blocking the Cro battery:

- **`CBOR::Simple`**: needs parameterized `array[...]` types (a general
  mutsu gap) and has broader diagnostic/malformed-input failures. Cro only
  exercises the narrow slice `Log::Timeline`'s CBOR output backend uses.
  Ticket: [`cbor-simple-typed-array-and-diagnostic-format-gaps.md`](../../todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md).
- **`IO::Socket::Async::SSL`**: `IO::Socket::Async::Listener` is missing a
  `.Supply` method in mutsu, so every test that opens a real listening
  socket fails. `Cro::HTTP`'s own suite drives its `Cro::Transform`
  pipelines directly (via `Supplier`), not through `Cro::HTTP::Server.start`,
  so this does not surface there — but it would for an actually-deployed
  HTTPS/HTTP2 Cro server. Ticket:
  [`io-socket-async-listener-supply-method-missing.md`](../../todo/tickets/io-socket-async-listener-supply-method-missing.md).
- **`Log::Timeline`**: its CBOR/JSON-lines/socket output backends produce
  wrong data (a lost event, an unexpected `Any`). Cro's own timeline
  instrumentation usage passes; the gap is in `Log::Timeline`'s own output
  round-trip suite. Ticket:
  [`log-timeline-cbor-output-format-mismatch.md`](../../todo/tickets/log-timeline-cbor-output-format-mismatch.md).

None of these three is patched — they run unmodified, per the adoption
policy (rung 2: grow mutsu, never the module). Their whitelisted (currently
passing) files are still gated by the release-time
`scripts/battery-testsuite.sh`; a regression in even those would block a
release.

## Why these modules

Not "selected" from a candidate field — they are `Cro::HTTP`'s declared
`depends`, and the adoption policy says to run the genuine dependency rather
than route around it (same reasoning as [`http-deps.md`](http-deps.md) for
`HTTP::UserAgent`'s layer). The alternatives were therefore not competing
CBOR/JWT/logging libraries but *other ways to satisfy the dependency*:

- **Reimplement natively** — rejected: rung 3 is a last resort, and would
  fork the ecosystem's CBOR/JWT/logging surface for no gain.
- **Leave to `mzef install`** — rejected: defeats the batteries premise
  (Cro would be bundled but unusable without a network install step).

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump a module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `TinyFloats` | <https://github.com/japhb/TinyFloats> | 0.0.5 (HEAD, no matching tag yet) | `ee901975` (2025-05-13) |
| `CBOR::Simple` | <https://github.com/japhb/CBOR-Simple> | 0.1.4 | `48104256` (2025-05-24) |
| `IO::Socket::Async::SSL` | <https://github.com/raku-community-modules/IO-Socket-Async-SSL> | 0.8.2 | `93de2580` (2025-06-03) |
| `JSON::JWT` | <https://github.com/raku-community-modules/JSON-JWT> | 1.1.2 | `ca573c81` (2024-12-11) |
| `Log::Timeline` | <https://github.com/raku-community-modules/Log-Timeline> | 0.5.2 | `1d9dd580` (2024-11-30) |

```sh
# 1. Clone the new upstream revision, then copy the runtime tree + attribution.
#    Upstream tests/CI/dev-utility files (t/, examples/, dist.ini, run-tests)
#    are deliberately NOT vendored: the release gate fetches the tests fresh
#    at the pinned commit (BATTERIES.md §3).
rsync -a --exclude '.precomp' <checkout>/lib/ modules/<Dist>/lib/
cp <checkout>/{META6.json,LICENSE,README.md,Changes} modules/<Dist>/   # those that exist

# 2. Bump the module's `commit` row in batteries.lock and the table above.
# 3. Re-run the gate and review the diff (a newly failing file is a
#    regression to fix, not to whitelist away):
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt

# 4. Refresh the Pages manifest:
python3 scripts/gen-batteries-manifest.py
```

## API sketch

```raku
use JSON::JWT;
my $token = JSON::JWT.encode({ sub => 'alice' }, alg => 'HS256', key => 'secret');
my %claims = JSON::JWT.decode($token, key => 'secret');

use Log::Timeline;
class MyTask does Log::Timeline::Task['MyApp'] { }
{
    Log::Timeline::Event[MyTask, 'started'].log;
    my $timed = Log::Timeline::Timed[MyTask, 'working'].start;
    LEAVE $timed.end;
}
```

## License

- `TinyFloats`, `CBOR::Simple`, `IO::Socket::Async::SSL`, `Log::Timeline` —
  Artistic-2.0. `JSON::JWT` — MIT.
- Vendored verbatim with `LICENSE` / `META6.json` / `README` / `Changes`
  preserved for attribution, source unmodified (per
  [BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
