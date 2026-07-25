# Bundle the real `JSON::Tiny` instead of emulating it (and size the `JSON::Fast` case)

mutsu recognizes `use JSON::Fast` and `use JSON::Tiny` as built-in modules and
serves both from one native implementation (`src/runtime/json.rs`, dispatched in
`vm_native_json.rs`). That is a **rung-3 private reimplementation**, which the
[adoption policy](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is)
treats as a last resort. Two of the emulated modules should be revisited, and
they have very different price tags.

## `JSON::Tiny` — bundleable today (measured 2026-07-25)

Upstream <https://github.com/moritz/json> is **pure Raku with zero `nqp::` use**
(a grammar plus an actions class), and it **already runs on mutsu unmodified**.
Against a plain checkout (`mutsu -I lib`), its own suite gives:

| File | Result |
| --- | --- |
| `01-parse.t` | 92/93 |
| `02-structure.t` | 10/10 |
| `03-unicode.t` | 4/4 |
| `04-roundtrip.t` | 17/17 |
| `05-utf16.t` | 2/2 |
| `06-meta-valid.t` | 9/9 |

The single failure is subtest 93, `throws-like X::JSON::Tiny::Invalid` — the
module's own exception class not matching. Fix that and the dist is a clean
`modules/JSON-Tiny/` battery with its own `batteries.lock` row, at which point
`use JSON::Tiny` runs genuine community code instead of an emulation.

## `JSON::Fast` — a real campaign, not a slice

Upstream <https://github.com/timo/json_fast> is the opposite: `lib/JSON/Fast.pm6`
contains **389 `nqp::` calls across 52 distinct ops**. They are individually
simple (`iseq_i`, `add_i`, `push_s`, `bindpos`, `ordat`, `eqat`, `substr`,
`chars`, `splice`, `list_i`, `create`, `while`, `stmts`, …) — low-level int /
str / array primitives rather than exotic VM internals — so this is bounded
work, but it is an `nqp::` op-surface project, not a vendoring task.

It also matters more than `JSON::Tiny`: `JSON::Fast` is what the ecosystem
actually depends on (`modules/OpenSSL`, `modules/HTTP-UserAgent` and
`vendor/zef/lib/Zef/Distribution.rakumod` all name it). Any move here must keep
the native fast path working for them.

## Also emulated, for the record

- `Pod::To::Text` — `pod2text` over the native Pod tree (`runtime/io_pod.rs`).
  The real dist is small; worth the same check as `JSON::Tiny`.
- `Test` / `Test::Tap` — `runtime/test_functions.rs`. This is Rakudo *core*
  (`lib/Test.rakumod`), not an ecosystem dist, so "bundle it" is a different
  question from the two above.
- `NativeCall` — must stay built-in: the `is native(...)` trait machinery is the
  VM's (`runtime/nativecall.rs`), and the `use` is only a recognized no-op.
- The pragmas (`strict`, `MONKEY-*`, `nqp`, `fatal`, …) — built-in by nature.

## Related wart

A missing `Test::*` module is **silently tolerated** at `use` time
(`runtime/runtime_module.rs`, the `module.starts_with("Test::")` branch). That is
why `HTTP::UserAgent`'s `070-ua-simple.rakutest` passes with no
`IO::Capture::Simple` present — its body is behind `NETWORK_TESTING`, so the
unresolved `use Test::IO::Capture` never bites. It should not stay silent
forever.
