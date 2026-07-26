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

**Re-verified 2026-07-26** (zero `nqp::`; 01-parse 92/93, 02 10/10, 03 4/4, 04
17/17, 05 2/2 — same picture).

### …but measure the speed before switching the default

**Correctness is not the blocker; throughput is.** Parsing a 3 KB META-shaped
document 200 times:

| implementation | time |
| --- | --- |
| native built-in (`runtime/json.rs`) | **0.49 s** |
| the real JSON::Tiny (grammar + actions) | **did not finish in 600 s** |

i.e. **>1000x slower**, because it is a Raku grammar running on mutsu's regex
engine rather than a Rust parser. JSON sits on zef's metadata path (every
`META6.json`, every index read), so swapping the *default* `use JSON::Tiny` to
the vendored source would be a serious regression even though every test passes.

**Measurement trap** (cost an hour on 2026-07-26): `runtime_module.rs` intercepts
`use JSON::Fast` / `use JSON::Tiny` **before** the `-I` search path, so
`mutsu -I <dist>/lib` still runs the built-in. Any "does the real one work / how
fast is it" check must rename the module first (e.g. `JSON::Tiny` → `JT::Real`)
to bypass the interception. Both of this session's first-pass conclusions about
JSON were wrong because of it.

So the realistic shape is: vendor the real `JSON::Tiny` as a battery **and keep
the native implementation as the fast path**, rather than deleting the emulation
— or make the swap conditional on mutsu's grammar/regex engine getting much
faster. That is a decision to take deliberately, not a cleanup.

## `JSON::Fast` — a real campaign, not a slice

Upstream <https://github.com/timo/json_fast> is the opposite: `lib/JSON/Fast.pm6`
contains **389 `nqp::` calls across 52 distinct ops**, and it is an `nqp::`
op-surface project, not a vendoring task.

**Corrected 2026-07-26 — "individually simple / bounded work" was wrong.** Of the
51 ops it uses, **42 are missing**, and they do not all belong to the easy tier:

| tier | n | examples | difficulty |
| --- | --- | --- | --- |
| A. pure data ops | 19 | `add_i` `concat` `substr` `eqat` | mechanical, one small function each |
| B. native typed arrays | 10 | `list_i` `push_i` `bindpos` `splice` | needs a native buffer representation |
| C. **control structures** | 6 | `if` `unless` `while` `until` `stmts` `ifnull` | take *thunks* — **cannot be builtins**, needs compiler lowering |
| D. **representation / meta** | 7 | `null` `create` `getattr` `p6bindattrinvres` | needs a null sentinel distinct from Nil/Any, and uninitialised P6opaque storage |

`while` and `stmts` were listed above as "individually simple"; they are not
functions at all. And per dist this is a **threshold function** — implementing
80% of the ops still leaves the module dead — so there is no cheap partial win.

Weigh that against what it buys: `JSON::Fast` carries 1439 reverse-deps (12.6% of
all ecosystem dependency weight), but mutsu **already answers `use JSON::Fast`
natively**, so implementing the 42 ops would change nothing a user can observe.
See `news/2026-07/nqp-op-layer-measured-and-rejected.md` for the full measurement and the
conclusion that bundling/emulating the few nqp-heavy hubs is the cheaper
strategy.

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
