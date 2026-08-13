# Cro is bundled — the web-framework battery slot is filled

`Cro::Core`, `Cro::TLS`, and `Cro::HTTP` (v0.8.10/0.8.10/0.8.13, all
`auth<zef:cro>`, Artistic-2.0) now ship in mutsu's `modules/` tree with
zero-config `use` — no `zef install`, no `-I` flags, no network access. This
fills the last hole in the batteries yardstick ("a small web blog can be
written with the bundle alone", `BATTERIES.md` §2): a program using nothing
but the shipped binary can now build an HTTP(/2)/WebSocket server, router,
middleware stack, sessions, and bearer-token auth.

The bundling decision had a standing precondition, set 2026-07-31
(`docs/batteries/cro-http.md`, then `web-framework.md`): "do not bundle Cro
yet; treat 'Cro::HTTP suite green under mutsu' as the campaign target." That
target was reached 2026-08-13 — a fast-dispatch method-cache bug fix
(`fast-method-cache-drops-attributive-param-writeback.md`) closed the last
open item, taking Cro::HTTP's upstream suite from 1/28 (2026-07-31) to 35/35
(Cro::Core stayed at 9/9 throughout) — the finish line of roughly 100
sessions of general interpreter fixes across cross-thread
`Supply`/`whenever`/closure semantics, method dispatch, typed lexicals, and
parser edge cases (none of it Cro-specific; see the many `news/2026-07/` and
`news/2026-08/` entries from that campaign). With the target met, bundling
was confirmed with the user and executed the same day.

Five new supporting dependencies were vendored alongside Cro itself:
`TinyFloats`, `CBOR::Simple`, `IO::Socket::Async::SSL`, `JSON::JWT`, and
`Log::Timeline` (`docs/batteries/cro-deps.md`) — every other `Cro::HTTP`
dependency (`OO::Monitors`, `Crypt::Random`, `IO::Path::ChildSecure`,
`Base64`, `HTTP::HPACK`, `Digest::HMAC`, `DateTime::Parse`, native
`JSON::Fast`) was already bundled ahead of this campaign. `TinyFloats` and
`JSON::JWT` fully pass their own upstream suites; `CBOR::Simple`,
`IO::Socket::Async::SSL`, and `Log::Timeline` are sufficient for what
Cro::HTTP itself needs (proven by its own 35/35) but each has a narrower gap
in its *own* broader upstream suite, filed as separate follow-up tickets
(`todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md`,
`todo/tickets/io-socket-async-listener-supply-method-missing.md`,
`todo/tickets/log-timeline-cbor-output-format-mismatch.md`) rather than
blocking the Cro battery.

All 8 new dists are registered in `batteries.lock` (44 Cro/Cro::Core test
files plus 15 from the new supporting deps — 47 new whitelist entries total,
purely additive, no existing battery regressed) so the release-time gate
(`scripts/battery-testsuite.sh`, run by `release.yml`) re-runs every one of
them against the shipped `mutsu` on every release; a regression blocks the
release. Nothing upstream was patched — every module runs verbatim, per the
adoption policy.
