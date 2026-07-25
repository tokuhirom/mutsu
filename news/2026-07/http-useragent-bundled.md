# `HTTP::UserAgent` is bundled — mutsu ships a working HTTP client

The HTTP client slot is filled. `HTTP::UserAgent` v1.2.0 is vendored verbatim at
`modules/HTTP-UserAgent/` and resolves with **zero config** — no `-I`, no
install, no environment setup:

```raku
use HTTP::UserAgent;
say HTTP::UserAgent.new.get('https://example.com/').code;   # 200
```

That closes the bottom-up build order the battery plan set out: TLS foundation
(`OpenSSL` + `IO::Socket::SSL`) → dependency layer (`URI`, `MIME::Base64`,
`HTTP::Status`, `DateTime::Parse`, `Encode`, `File::Temp`,
`File::Directory::Tree`) → client. A program can now make a real `https://`
request using nothing but the shipped binary.

## Vendored, not patched

Per the [adoption policy](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is)
the module ships unmodified: `lib/` plus `META6.json`, `LICENSE` (MIT),
`README.md` and `Changes` for attribution; upstream `t/`, `xt/`, `doc/`,
`examples/`, CI config and `.precomp` artifacts are excluded. Every gap the
upstream suite exposed was closed in the *interpreter*, not in the vendored copy
— the whole campaign is recorded across this month's entries, ending with the
`subset` package-qualification and `throws-like` Regex-matcher fixes that took
the suite to 27/27.

No wiring was needed to make the bundle resolvable: `resolve_bundled_lib_paths`
enumerates `modules/*/lib` dynamically, and the release tarball and container
already `cp -R modules` into `share/mutsu/modules`.

## The release gate

`batteries.lock` gained an `HTTP::UserAgent` row pinned at upstream `1d6a31a0`
(v1.2.0, 2025-05-04), and `batteries-whitelist.txt` grew from 53 to **80** files
— every one of which passes.

**26 of the suite's 27 files are gated.** Three of them almost were not:
`110-redirect-cookies`, `230-binary-request` and `250-issue-144` need
`Test::Util::ServerPort`, a **test-only** dependency, and
`scripts/battery-testsuite.sh` cannot fetch one — it clones only the battery's
own repository, so the files died with `ok=0` (module missing, not a failing
assertion). Rather than leave the redirect and binary-upload paths ungated —
exactly the paths most likely to regress — that helper is now bundled too; see
[test-helpers.md](../../docs/batteries/test-helpers.md) for why it earns its
place beyond this suite. `HTTP::UserAgent`'s two other `test-depends` needed no
bundling, though for different reasons: `JSON::Fast` really is provided
natively (`runtime/json.rs`), whereas `IO::Capture::Simple` is not —
`070-ua-simple` passes only because its whole body sits behind
`NETWORK_TESTING` and mutsu tolerates a missing `Test::*` module at `use` time.

## New: the gate never runs third-party-service tests

The one ungated file, `082-exceptions`, exposed a real hole in the gate. It
makes **unguarded live requests to `httpbin.org`** — and `httpbin.org` spent part
of 2026-07-25 returning 503, exactly while this battery was being bundled. A file
like that in a release-blocking gate means someone else's outage can block a
mutsu release that has nothing wrong with it.

So the harness grew an exclusion list,
[`batteries-exclude.txt`](../../batteries-exclude.txt): `name<TAB>testfile`
entries that are skipped entirely, in both gate and `--update` mode, so they can
neither fail a release nor re-enter the baseline. A whitelisted-but-excluded file
reports a `note:` telling you to re-run `--update`, not a regression; the gate's
teeth are otherwise unchanged (verified both ways against a scratch
manifest/baseline).

Which files belong there was **measured, not guessed**: every whitelisted file
was re-run inside a loopback-only network namespace
(`unshare -rn -- sh -c 'ip link set lo up; …'`). Exactly two failed —
`HTTP::UserAgent/082-exceptions`, and `IO::Socket::SSL/01-basic`, which connects
to `github.com:443` in its first two lines, *before* its own `NETWORK_TESTING`
guard. Everything else already routes its live-network assertions through
`NETWORK_TESTING`, which the gate does not set. Those two are the whole
exclusion list, and with them out, **all 80 baseline files pass offline** — the
gate's verdict no longer depends on any third-party service.

Fetching each suite at its pinned commit still needs the network, of course —
that is setup, and a fetch failure reports `GATE ERROR` (exit 2), distinct from
the `GATE FAILED` a real regression produces.
