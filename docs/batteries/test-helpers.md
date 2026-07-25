# Battery: test helpers — `Test::Util::ServerPort`

**Slot:** Test helpers · **Chosen:** `Test::Util::ServerPort`
(`auth<zef:jonathanstowe>`, v0.0.5, Artistic-2.0) · **Kind:** Adopted (community
module, vendored as-is)

## What it is

One exported subroutine:

```raku
use Test::Util::ServerPort;
my $port = get-unused-port();          # or get-unused-port(20000 .. 30000)
```

It picks a random port in the range and confirms it is bindable by actually
listening on it, retrying until one is free. Zero dependencies, ~30 lines.

## Why it is bundled

Two reasons, and the second is what tipped it.

**It makes the bundle self-sufficient for testing a server.** The batteries goal
is "a small web blog can be written with the bundle alone." Writing one implies
*testing* it, and any test that starts a listener needs a free port; hardcoding
one makes the suite fail the moment two runs overlap. mutsu's own suite learned
this the hard way (`t/io-socket-recv-limit.t` was misdiagnosed as flaky for
months when it was really a port collision — see "Known flaky tests" in
CLAUDE.md). Shipping the helper means a user's test suite does not have to
re-solve it.

**It closes a hole in the release gate.** `HTTP::UserAgent` declares it under
`test-depends`, and three of that suite's files use it:
`110-redirect-cookies`, `230-binary-request`, `250-issue-144` — the redirect and
binary-upload paths, exercised against a local `TestServer`. Those are precisely
the paths most likely to regress, and before this they could not be gated at all:
`scripts/battery-testsuite.sh` fetches only the battery's own repository, so a
test-only dependency that is not bundled is simply unavailable and the files die
with `ok=0`. Bundling the helper turns them into real, offline, deterministic
gate coverage.

`HTTP::UserAgent`'s two other `test-depends` did not need bundling, but for
different reasons. `JSON::Fast` really is provided — natively, by
`runtime/json.rs` — so `080-ua` exercises it for real. `IO::Capture::Simple` is
**not**: `070-ua-simple` passes because its entire body sits behind
`NETWORK_TESTING`, and mutsu tolerates a missing `Test::*` module at `use` time
(`runtime/runtime_module.rs`), so the unresolved `use Test::IO::Capture` never
bites. Setting `NETWORK_TESTING` would fail on `prints-stdout-ok`. That
tolerance is a wart worth revisiting, not a feature to rely on.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `Test::Util::ServerPort` | <https://github.com/jonathanstowe/Test-Util-ServerPort> | v0.0.5 | `f7678985` (2023-07-05) |

What is vendored: `lib/` plus `META6.json`, `LICENCE`, `README.md`, `Changes`
for attribution. Upstream `t/`, `.github/` and precomp artifacts are excluded —
the release gate fetches the tests fresh at the pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/Test-Util-ServerPort/lib/
cp <checkout>/{META6.json,LICENCE,README.md,Changes} modules/Test-Util-ServerPort/
# then bump batteries.lock, re-run the gate, refresh the Pages manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use Test::Util::ServerPort; my $p = get-unused-port(); say 1024 < $p < 65536'   # True
```

## License

**Artistic-2.0** — declared in `META6.json` and shipped as `LICENCE`
(Copyright (c) 2016 Jonathan Stowe). Vendored verbatim with its `LICENCE` /
`META6.json` / `README` preserved for attribution, source unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
