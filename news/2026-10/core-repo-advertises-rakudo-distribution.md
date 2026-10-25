# The `core` repository advertises a `rakudo` distribution

Rakudo's `core` Installation repository holds one distribution named `rakudo`, at the compiler's
version. mutsu's `core` repo held the bundled `Rakudo-Core` battery under its own name and a stale
`2026.06`, so `Pakku::Spec.new('rakudo')` (Pakku's `t/core.rakutest` test 6, and any
`depends: ["rakudo:ver<...>"]`) was unsatisfied.

`bundled_core_distribution` now presents the battery's META as `name` `rakudo` with `ver` (and
`version`) derived from `RAKUDO_COMPAT_VERSION`, the same coordinate ADR-0104 uses for
`$*RAKU.compiler.version`, so the two cannot disagree and a version bump moves both. The vendored
`modules/Rakudo-Core/META6.json` and the battery's directory name are untouched, and
`$*RAKU.compiler.name` stays `mutsu`. Core modules still resolve by short name (`Test`), now
reporting the distribution as `rakudo`; `site` and `home` hold nothing named `rakudo`.
