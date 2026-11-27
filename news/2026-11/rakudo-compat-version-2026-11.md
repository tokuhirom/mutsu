# `$*RAKU.compiler.version` is now v2026.11

`Interpreter::RAKUDO_COMPAT_VERSION` moved from `2026.09` to `2026.11` (ADR-0104). That is the
release `.rakudo-version` now pins for the Rakudo oracle, the bench baseline and the ecosystem
sweep. The `Perl6::SysConfig` `version` key and the core repo's `rakudo` distribution follow it.

As ADR-0104 asks, the claim is backed by a measurement first: the roast reference baseline
(`docs/data/raku-baseline.tsv`) was re-captured on Rakudo 2026.11. Nine files changed
`raku_status`, all of them whitelisted:

- three that rakudo now passes, among them `S02-types/array-shapes.t` after 2026.11's
  shaped-array rework;
- four where rakudo now fails or no longer compiles a line.

Every file rakudo passes unfudged is now on mutsu's whitelist. The refresh notes are in
`docs/data/raku-baseline.md`.
