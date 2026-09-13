# `JSON::Fast` is a bundled battery, and the native provider is deleted

The last unjustified rung-3 native provider is gone. `use JSON::Fast` now loads
`modules/JSON-Fast/lib/JSON/Fast.pm6` — upstream `timo/json_fast` tag `0.20.1`,
vendored byte-identical — through the ordinary module-resolution ladder, like
any other battery. All **14** of its own upstream test files, **931
assertions**, pass against the bundled copy.

This closes the campaign [#8226](https://github.com/tokuhirom/mutsu/issues/8226)
opened, and with it ADR-0096 §D4/E2. **That ledger is now one entry:
`NativeCall`.**

## What was deleted

`use JSON::Fast` used to be answered by a native Rust implementation selected by
the module's *name*. [#8183](https://github.com/tokuhirom/mutsu/issues/8183)
demoted it from an interception to a last-resort fallback and took `JSON::Tiny`
off it entirely; this removes what was left:

- the `json_native_provider` flag and `json_native_provider_active()`;
- `try_native_json_function` and its four dispatch sites (`runtime/calls.rs`,
  `vm/vm_call_func_ops.rs`, `runtime/builtins_operators_fallback.rs`,
  `runtime/accessors_resolve.rs`);
- `JsonImportDefaults`, which existed only to model the
  `use JSON::Fast <immutable !pretty>` import list.

The proof that nothing name-keyed survives is not a code reading: move
`modules/JSON-Fast` aside and `use JSON::Fast` answers *"Could not find
JSON::Fast in: (module repositories)"*, like any other missing module.

## What is still native, and why it is not an exception

`src/runtime/json.rs` and `src/vm/vm_native_json.rs` survive, narrowed to one
caller: `Rakudo::Internals::JSON.to-json` / `.from-json`.

That is **core Rakudo surface, not a module**. `raku -e 'say
Rakudo::Internals::JSON.to-json({a => 1})'` resolves with no `use`, so
implementing it in Rust is a builtin like any other and ADR-0096 §D4's ledger
does not reach it. It is also load-bearing: zef reads every `META6.json` through
it (`vendor/zef/lib/Zef.rakumod`), as do OpenSSL's `%?RESOURCES` loading and
JSON::JWT.

This is a correction to #8226's own step 4, which said to delete both files
outright. Doing that literally would have broken zef.

## Why this module and not the other one

Of the 1,625 distributions in the `ecosystem/` ledger, **541** carry
`JSON::Fast` in their resolved dependency closure, against `JSON::Tiny`'s
**32**. Inside the tree the split is starker: five `use JSON::Fast;` statements
across three bundled batteries (`Cro::HTTP`, `JSON::JWT`, `Log::Timeline`) and
six `META6.json` declarations — while **nothing bundled `use`s `JSON::Tiny`**.
For a year the ecosystem's default JSON module was the one mutsu did not ship.

`JSON::Tiny` stays bundled. Whether it should is a BATTERIES.md §2 selection
question for the maintainer, and it is cheap to keep: it is the reference
implementation, a real vendored module exercising the grammar engine, and its
own six upstream files pass.

## What it took

Not the recorded blocker. Six places in the tree said the real distribution
"depends on ~50 `nqp::` ops mutsu does not implement"; nobody had run it. Probed
op by op, **42 of its 51 ops already worked** and the module parsed completely,
dying at runtime on the first of nine missing ops. The 2026-07 measurement that
produced the "42 missing" figure was describing a tree that no longer existed —
its premise ("mutsu already ships its own JSON::Fast, so the real distribution
never runs") had been removed by #8203, and its verification repro (`dies with
Unknown function: list_i`) had stopped reproducing months earlier.

The nine ops were the *first* stop, not the last. What they were hiding, and
what the suite turned up after them, were all general interpreter bugs:

| | |
|---|---|
| `Uni` was not a codepoint store | `to-json` emitted **unescaped, invalid JSON** for any string with a quote or control character in it — the escaper's scan loop never ran an iteration |
| `nqp::create` allocated no storage | `Mu.CREATE`'s attribute-less instance was unreachable for `nqp::bindkey`/`nqp::push` |
| storage-object installs | `'$!reified'`/`'$!storage'` had to unify two stores, in both fill-then-install and install-then-fill orders |
| `nqp::strfromcodes` did not normalize | every string round-tripped through `.NFD` came back decomposed |
| deep recursion aborted the process | [#8232](https://github.com/tokuhirom/mutsu/issues/8232) / ADR-0100 — a Rust stack overflow with nothing for `try` to catch |
| `++$p` lost its native `is rw` reference | [#8233](https://github.com/tokuhirom/mutsu/issues/8233) — `from-json('{"a":"b"}/')` silently accepted trailing garbage |
| a brace classified by what followed it | [#8282](https://github.com/tokuhirom/mutsu/issues/8282) — `f({ "a" => 1 }, <x y>)` was a Block where `f({ "a" => 1 })` was a Hash |
| a conditional lost a native reference | #8282 — `f($c ?? $p !! ++$q)` |
| `Rational[Int,Int]` could not say its value | #8282 — `.Str` rendered the type object into the JSON |

Every one of those is a fix for mutsu, not for JSON. That is the shape ADR-0096
§D3 asks for: grow the interpreter until the real module runs, rather than ship
something that only looks like it.

## Files

- `modules/JSON-Fast/` — vendored `lib/`, `META6.json`, `LICENSE`, `README.md`
- `docs/batteries/json-fast.md` — the battery record, with the re-vendoring recipe
- `batteries.lock`, `batteries-whitelist.txt` — the release gate's 14 files
- `t/modules/batteries/json-module-ladder.t` — now pins both modules resolving
  through the ladder, and that `Rakudo::Internals::JSON` survived the removal
- `docs/adr/0096-batteries-adoption-policy.md` §D4/E2 — closed
