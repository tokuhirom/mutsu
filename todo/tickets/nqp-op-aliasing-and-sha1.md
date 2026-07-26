# `nqp::` leftovers: `nqp::sha1` blocks vendored zef, and qualified calls still alias builtins

Supersedes `todo/deep/nqp-op-layer-missing.md`, whose framing ("build an `nqp::`
op layer, ~53 ops missing, needs an ADR") did not survive measurement. What
follows is what was actually measured on 2026-07-26.

## Measured: how big is the `nqp::` question, really

Over the 847 cached fez dists:

| | count | share |
| --- | --- | --- |
| use `nqp::` (excluding deep-guts signals) | 64 | **7.6% of dists** |
| their share of reverse-dependency weight | 2322 / 11403 | **20.4%** |

The weight number looks alarming because `JSON::Fast` alone carries 1439
reverse-deps (12.6% of all weight). **But mutsu already ships its own
JSON::Fast** — `runtime_module.rs` intercepts `use JSON::Fast` / `use JSON::Tiny`
— so the real distribution never runs, and implementing its ops would change
nothing a user can observe. (Verified: the real source, renamed to bypass the
interception, dies with `Unknown function: list_i`.)

**Implementing "just what JSON::Fast needs" is therefore the worst available
target**, and it is not small either: of the 51 ops it uses, 42 are missing, and
they span every difficulty tier —

| tier | n | examples | difficulty |
| --- | --- | --- | --- |
| A. pure data ops | 19 | `add_i` `concat` `substr` `eqat` | mechanical, one small function each |
| B. native typed arrays | 10 | `list_i` `push_i` `bindpos` `splice` | needs a native buffer representation |
| C. **control structures** | 6 | `if` `unless` `while` `until` `stmts` `ifnull` | take *thunks* — cannot be builtins, needs compiler lowering |
| D. **representation / meta** | 7 | `null` `create` `getattr` `p6bindattrinvres` | needs a null sentinel distinct from Nil/Any, and uninitialised P6opaque storage |

Per dist this is a **threshold function**: implementing 80% of a module's ops
still leaves it dead. That is what makes a general "demand-driven, op at a time"
plan weak.

**Conclusion: do not build an `nqp::` layer.** For the small number of
nqp-heavy hub modules, bundling/emulating them (what mutsu already does for
JSON::Fast, and what `todo/tickets/bundle-json-tiny-instead-of-emulating.md`
proposes) is far cheaper than the layer.

## Done: unimplemented `nqp::` ops no longer alias a Raku builtin

`call_function_fallback` strips a package prefix and retries the short name, so
`nqp::index("hello", "z")` reached Raku's `index` and returned **Nil** where nqp
yields **-1** — a silent wrong answer, and nqp code branches on exactly that
(`!= -1`). An unimplemented `nqp::` op now fails with
`Unsupported nqp:: op: nqp::<name>`. Pinned by
`t/nqp-unimplemented-op-errors.t`.

## Open 1 — `nqp::sha1` is needed by TWO things we already ship

This is the one *real* demand found, and it is entirely in our own tree:

```
vendor/zef/lib/Zef/Distribution.rakumod:230       return nqp::sha1(self.Str);
vendor/zef/lib/Zef/CLI.rakumod:761,797,808,820    nqp::sha1(...)
modules/OpenSSL/lib/OpenSSL/NativeLib.rakumod:33  my $content-id = nqp::sha1($resource-name);
```

- **zef**: `Zef::Distribution.id` plus the source-path computation — the **mzef**
  critical path (PLAN §1 B2).
- **bundled OpenSSL**: `dll-resource()` hashes a resource name to find its
  unmangled copy under `$*TMPDIR`. `use OpenSSL` succeeds today only because that
  sub is not called at load time.

mutsu has no SHA-1 at all — nothing in tree and no `sha`/`digest` crate in
`Cargo.toml` — so this needs either ~60 lines of SHA-1 or a dependency, then
wiring `nqp::sha1` as a full-name builtin beside `nqp::ordat` /
`nqp::gethostname` / `nqp::bindattr`.

Small, self-contained, and unblocks a north-star path in code we already ship.
**Best next `nqp::` work by far** — and note the contrast: the "obvious" target
(`JSON::Fast`, 42 ops) would unlock nothing, while this one op unblocks two
shipped components.

## Which bundled modules exist, and why (measured 2026-07-26)

Useful context for anyone weighing "bundle vs implement":

- `modules/` holds **12 vendored real Raku distributions** (DateTime::Parse,
  Encode, File::Directory::Tree, File::Temp, HTTP::Status, HTTP::UserAgent,
  IO::Socket::SSL, MIME::Base64, OpenSSL, Template::Mustache,
  Test::Util::ServerPort, URI). These are batteries chosen by PLAN §B1's
  criterion, **not** for nqp reasons — only OpenSSL touches `nqp::` at all, in the
  one line above.
- Rust-native replacements intercepted in `runtime_module.rs`: **`JSON::Fast` /
  `JSON::Tiny` are the only ones bundled *because of* nqp** (the code comment says
  so: the real distributions need ~50 ops mutsu lacks). `Pod::To::Text`
  (`io_pod.rs`), `Test` / `Test::Util` (`test_functions.rs`) and `NativeCall` are
  native for unrelated reasons — the data they work on is already an internal
  representation.

## Open 2 — a qualified call still falls back to a builtin (wider than nqp)

Same mechanism, outside `nqp::`:

```raku
say Foo::Bar::index("hello", "l");
# raku:  Could not find symbol '&index' in 'GLOBAL::Foo::Bar'
# mutsu: 2
```

The short-name retry in `call_function_fallback` is load-bearing — it is how a
call qualified with a package mutsu did not register still finds its routine — so
narrowing it needs a measurement of what actually depends on it. Note the
obvious guard does **not** work: `index` is dispatched by a hand-written arm in
`call_function`, not via `BUILTIN_FUNCTION_NAMES`, so `is_builtin_function` does
not recognise it. Deferred for that reason, not because it is unimportant.

## Files

- `src/runtime/builtins_operators_fallback.rs` — the `nqp::` guard and the
  package-prefix strip that follows it
- `src/runtime/builtins.rs` — the full-name `nqp::` ops (`atkey`, `atpos`,
  `ordat`, `gethostname`, `bindattr`)
- `src/runtime/runtime_module.rs` — `use nqp` as a no-op pragma; the
  JSON::Fast / JSON::Tiny interception
