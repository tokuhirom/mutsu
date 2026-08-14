# Vendor the real `JSON::Tiny` as a battery, keep the native fast path

`todo/tickets/bundle-json-tiny-instead-of-emulating.md` measured (2026-07-25)
that upstream `JSON::Tiny` (`moritz/json`) is pure Raku, zero `nqp::` use, and
already runs on mutsu unmodified — but that the real grammar is over 1000x
slower than mutsu's native `to-json`/`from-json` (`src/runtime/json.rs`) on a
representative document, which matters because JSON sits on zef's metadata
path. The recommended shape was to vendor the module as a battery while
keeping the native implementation as the default fast path, rather than
deleting the emulation.

That is what this change does:

- `modules/JSON-Tiny/` now carries the real upstream `lib/` (v1.0, commit
  `a5ef8c17`, Artistic-2.0) plus `META6.json`/`README.md` for attribution, a
  `batteries.lock` row, and a selection record
  (`docs/batteries/json-tiny.md`).
- `use JSON::Tiny;` (the bare module name) is still intercepted and answered
  natively, unchanged — the perf cliff that motivated keeping the emulation
  is real and this does not touch it.
- `use JSON::Tiny::Grammar;` / `use JSON::Tiny::Actions;` are **not**
  intercepted (only the two bare top-level module names are special-cased),
  so code that reaches for the grammar/actions classes directly — as
  `JSON::Tiny`'s own upstream test suite does — now resolves them from the
  real bundled module instead of failing to load (or requiring a manual
  `git clone` + `-I`, as mutsu's pre-existing `t/json-tiny-compat.t` did).
- The one genuine correctness gap this surfaced: the real `JSON::Tiny.from-json`
  throws `X::JSON::Tiny::Invalid` (carrying `.source`, with a `.message`
  computed from its length) on a parse failure, while mutsu's native path
  always threw a plain `X::AdHoc` (correct for `JSON::Fast`, which really does
  just `die` a string, but not for `JSON::Tiny`). `native_from_json` now picks
  the exception shape based on which module was `use`d — the same pattern
  already used for `JSON::Fast`'s own `X::JSON::AdditionalContent`. Pin:
  `t/json-tiny-invalid-exception.t`.

All 6 upstream `JSON::Tiny` test files (135 subtests) pass against the bundled
copy through `scripts/battery-testsuite.sh`, added to `batteries-whitelist.txt`
with zero regressions elsewhere (245/271 unchanged).

The original ticket's other two items are not part of this change: the
`JSON::Fast` half was already measured and rejected as a `nqp::`-op campaign
in `news/2026-07/nqp-op-layer-measured-and-rejected.md` (native stays the only
implementation — bundling upstream would need ~50 missing `nqp::` ops for zero
observable benefit, since mutsu already answers `use JSON::Fast` natively);
`Pod::To::Text` was separately retired to a real vendored module per
`docs/batteries/pod-to-text.md`. The remaining "missing `Test::*` module is
silently tolerated" wart is split out to
`todo/tickets/missing-test-module-silently-tolerated.md`.
