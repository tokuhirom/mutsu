# Battery: JSON (reference) — `JSON::Tiny`

**Slot:** JSON (reference / `Grammar`+`Actions`) · **Chosen:** `JSON::Tiny`
v1.0 (`moritz/json`, Artistic-2.0) · **Kind:** Adopted (community module,
vendored as-is), with a **deliberate native fast path kept in front of it**

## What it is

A minimal, pure-Raku JSON (de)serializer — a grammar, an actions class, and
a thin `to-json`/`from-json` wrapper around them:

```raku
use JSON::Tiny;
my $json = to-json([1, 2, "a third item"]);
my $copy = from-json($json);
```

Three files (~120 lines), zero `nqp::` use, zero dependencies
(`todo/tickets/bundle-json-tiny-instead-of-emulating.md`, resolved by this
record).

## Why it is bundled, and why `use JSON::Tiny` still does NOT run it

mutsu already answers `use JSON::Fast` / `use JSON::Tiny` with a **native Rust
implementation** (`src/runtime/json.rs`, dispatched in
`src/vm/vm_native_json.rs`), gated at `use`-time in
`src/runtime/runtime_module.rs` (`use_module_with_tags_inner`) — the bare
module names `"JSON::Fast"` / `"JSON::Tiny"` are recognized and treated as a
no-op *before* the normal `-I` / `MUTSULIB` / bundled-module search even runs.
This was a rung-3 private reimplementation, justified because the real
`JSON::Fast` needs ~50 missing `nqp::` ops (`news/2026-07/nqp-op-layer-measured-and-rejected.md`).

`JSON::Tiny` is different: it is pure Raku and **already runs on mutsu
unmodified**. So why not just delete the emulation for it and let
`use JSON::Tiny` load the real thing? **Throughput.** Parsing a 3 KB
META-shaped document 200 times: the native implementation takes 0.49s; the
real `JSON::Tiny` grammar running on mutsu's regex engine did not finish in
600s (**>1000x slower**). JSON sits on zef's metadata path (every
`META6.json`, every index read) and is a default of many other bundled
batteries, so swapping the *default* `use JSON::Tiny` to the vendored Raku
source would be a serious regression even though every test passes.

So the shape this record ships is exactly what the ticket recommended:
**vendor the real module as a battery, and keep the native implementation as
the fast path** — the two are not the same thing, and `use JSON::Tiny` keeps
hitting native. Concretely:

- `use JSON::Tiny;` (the bare module name, anywhere) is still intercepted and
  answered natively — unchanged by this record.
- `use JSON::Tiny::Grammar;` / `use JSON::Tiny::Actions;` are **not**
  intercepted (only the two bare top-level names are special-cased), so code
  that reaches for the grammar/actions classes directly — as `JSON::Tiny`'s
  own upstream test suite does, and as mutsu's `t/json-tiny-compat.t` already
  did against a manually-cloned checkout — now resolves them from the bundled
  `modules/JSON-Tiny/lib` like any other battery.
- The vendored copy is also what a user gets if they explicitly shadow the
  bare name — e.g. `mzef install` a different `JSON::Tiny` version into the
  site repo, which (per BATTERIES.md §6) is layered *below* the native
  interception today, so that override path is not yet wired up. Recorded as
  a known gap below, not fixed by this record.

**Interpreter work it drove:** the one genuine correctness gap between mutsu's
native `from-json` and the real module was its exception shape on a parse
failure. The real `JSON::Tiny.from-json` throws `X::JSON::Tiny::Invalid`
(`.source` = the original text, `.message` computed from its length); mutsu's
native path threw a plain `X::AdHoc` (matching `JSON::Fast`, which really does
just `die` a string). Fixed by making `native_from_json` pick the exception
shape based on which module was `use`d (`self.loaded_modules.contains(...)`,
see `json_tiny_exception_style()` in `src/runtime/test_functions/mod.rs`) —
`JSON::Fast`'s own `X::JSON::AdditionalContent` mirroring
(`t/json-additional-content.t`) was the precedent. Pin:
`t/json-tiny-invalid-exception.t`.

Upstream tests: 6 files, 135 subtests (`t/04-roundtrip.t` has 10 expected
`TODO passed`) — all pass against the bundled `lib/`, matching raku. Smoke:
covered by `t/json-tiny-invalid-exception.t` and the pre-existing
`t/json-tiny-compat.t`.

## What is NOT fixed by this record

- **The native/vendored split for the bare module names is permanent policy,
  not a stopgap** — reversing it needs mutsu's regex/grammar engine to get
  much faster first (see the measurement above), which is a separate,
  large campaign.
- **`mzef`/site-repo overrides of `JSON::Tiny` do not shadow the native
  path.** BATTERIES.md §6 says an explicit `-I` / `MUTSULIB` / installed
  module should take priority over the bundled floor; the native interception
  currently jumps that whole ladder for the two JSON module names (like
  `Test`/`NativeCall`, which are recognized pragma-like built-ins by design).
  Not a regression introduced here — pre-existing behavior this record does
  not change — but worth flagging if a future security update to `JSON::Tiny`
  ever needs to reach a user who cannot rebuild mutsu.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `JSON::Tiny` | <https://github.com/moritz/json> | v1.0 | `a5ef8c17` (2017-10-24) |

What is vendored: `lib/` plus `META6.json` and `README.md` for attribution
(upstream ships no separate `LICENSE` file; the README carries the license
statement, same situation as `Crypt::Random`). Upstream `t/` and CI config
are excluded — the release gate fetches the tests fresh at the pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/JSON-Tiny/lib/
cp <checkout>/{META6.json,README.md} modules/JSON-Tiny/
# then bump batteries.lock, re-run the gate, refresh the Pages manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -I modules/JSON-Tiny/lib -e 'use JSON::Tiny::Grammar; say JSON::Tiny::Grammar.parse(q<{"a":1}>).defined'   # True
mutsu -e 'use JSON::Tiny; try { from-json "" }; say $!.^name'   # X::JSON::Tiny::Invalid
```

## License

**Artistic-2.0** — stated in `META6.json`'s `license` key and restated in the
upstream README ("can be used, modified and redistributed under the terms of
the Artistic License Version 2"). Vendored verbatim with `META6.json` /
`README` preserved for attribution, source unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
