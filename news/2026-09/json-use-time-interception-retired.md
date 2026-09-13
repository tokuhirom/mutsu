# The JSON `use`-time interception is retired

`use JSON::Tiny` now loads the vendored `JSON::Tiny` and runs its own Raku
source. `use JSON::Fast` still reaches mutsu's native `to-json`/`from-json`,
but only as a last-resort provider for a module mutsu does not ship: the
module-resolution ladder runs first, and whatever it finds wins. Closes
[#8183](https://github.com/tokuhirom/mutsu/issues/8183).

## What was there

mutsu recognized the bare module names `JSON::Fast` and `JSON::Tiny` at `use`
time and answered both from one native Rust implementation
(`src/runtime/json.rs`, dispatched in `src/vm/vm_native_json.rs`). The names
were hardcoded at three layers — the parser's export list
(`parser/stmt/simple/module_exports.rs`), `use`-time gating
(`runtime/runtime_module.rs`), and two dispatch sites — and the statement-position
site returned *before* `call_routine_def` specifically to beat the vendored
module's own resolved routines.

It was justified on throughput: 200 META-shaped documents parsed in 0.49s
natively against >600s through the real grammar on mutsu's regex engine. The
battery record declared the split permanent policy on that basis.

## Why it goes

Being slow is a reason to make something fast. It is not a reason to substitute
a semantically divergent implementation under the real module's name. The
divergence was observable three ways:

- `to-json([1, 2, "x"])` answered a pretty-printed block; the real module
  answers `[ 1, 2, "x" ]`.
- `from-json`'s exception type was picked by `json_tiny_exception_style()` —
  literally `JSON::Tiny loaded && !JSON::Fast loaded`, self-documented as a
  "best-effort guess". A program that loaded both got `JSON::Fast`-shaped errors
  from its `JSON::Tiny` calls, decided by an unrelated `use` elsewhere in the
  file.
- The whole resolution ladder was jumped for these two names, so an explicit
  `-I`, `MUTSULIB` or site-repo copy could not override them — the case that
  matters being an upstream security fix reaching a user who cannot rebuild
  mutsu.

Two measurements made during this work changed the picture the record was
resting on:

- **zef's metadata path was never on this.** `Zef::from-json` calls
  `Rakudo::Internals::JSON.from-json` (`vendor/zef/lib/Zef.rakumod:9`), a core
  Rakudo class mutsu implements natively as a genuine builtin and which this
  change does not touch. zef's only `JSON::Fast` mention is inside a `=begin
  pod` block. The sequencing worry that kept the mechanism unexamined — "zef's
  metadata path is on the regression side of this" — did not apply.
- **The gap shrank by ~48x.** The same 200-document parse through the real
  grammar takes **12.6s** today, against raku's 0.84s and the native path's
  0.011s. mutsu's grammar engine is ~15x off rakudo on this grammar rather than
  off the scale. That 15x is the honest bill and now stands on its own.

## What changed

- `JSON::Tiny` is gone from the mechanism entirely. It resolves through
  `use lib` → `-I` → `MUTSULIB` → site repo → the bundled floor like any other
  battery.
- `JSON::Fast` is not vendored (at the time this was written the real
  distribution was believed to need ~50 `nqp::` ops; that was never measured and
  turned out to be nine — see #8226 and
  `news/2026-09/json-fast-runs-for-real.md`),
  and five bundled batteries `use` it, so the native routines still answer it —
  after the ladder comes up empty. A new `json_native_provider` flag is set only
  on that fallback, and it is what gates native dispatch;
  `json_module_loaded()` (which read `loaded_modules`) is gone.
  **Superseded 2026-09-13**: `JSON::Fast` is a vendored battery now and the
  fallback, including that flag, is deleted — see
  `news/2026-09/json-fast-is-a-battery-and-the-provider-is-deleted.md`.
- Both dispatch sites now sit strictly after routine resolution. The
  statement-position override that matched on a def's package name is deleted,
  so a resolved def always wins.
- `json_tiny_exception_style()` and the `X::JSON::Tiny::Invalid` emulation are
  deleted. `JSON::Tiny` throws its own exception from its own source;
  the native provider throws JSON::Fast's plain `X::AdHoc`, unconditionally.

## Tests

`t/modules/batteries/json-tiny-compat.t` used to skip unless a `JSON::Tiny`
checkout had been cloned into `tmp/` by hand — it now runs against the bundled
battery, and all 48 of its assertions pass against the module's real source.
`t/exceptions/json-tiny-invalid-exception.t` asserts the module's own
`JSON::Tiny::X::JSON::Tiny::Invalid` (the composed name, matching raku) instead
of the native stand-in. The new
`t/modules/batteries/json-module-ladder.t` pins the architecture in seven
subprocess cases: the bundled battery answers a bare `use JSON::Tiny`, an `-I`
copy shadows it, the native provider answers an unresolvable `JSON::Fast`, an
`-I` `JSON::Fast` beats the native provider, and loading both modules no longer
changes either one's exception shape.

## One upstream assertion, and what it turned out to be

`JSON::Tiny`'s upstream `t/01-parse.t` is 92/93 and came off
`batteries-whitelist.txt`. Its last assertion reads
`X::JSON::Tiny::Invalid` as `throws-like`'s second argument, which evaluates
before the block containing `use JSON::Tiny` is invoked. Raku performs `use` at
BEGIN time and has the symbol; mutsu's `use` is a runtime opcode, so the
reference gets a fabricated stub that does not match the module's real
exception type.

That assertion passed before only because both sides were mutsu fabrications
agreeing with each other — the native path threw an exception named
`X::JSON::Tiny::Invalid`, and the parser pre-registered that bare name as a user
type. Retiring the interception did not break it so much as stop hiding it. The
underlying gap is general and has a JSON-free repro; it is filed as
[#8201](https://github.com/tokuhirom/mutsu/issues/8201), and fixing it is what
puts the file back on the whitelist.
