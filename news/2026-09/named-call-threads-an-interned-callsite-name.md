# The by-name call path threads an interned callsite name and package

[#7766](https://github.com/tokuhirom/mutsu/issues/7766) unit 2 item 1 (and,
because it is the same names one layer down, the `find_compiled_function` half
of item 4). A `Test` assertion went from **21.0 to 15.0 `Symbol::intern` calls
per assertion**, a 29% cut, with no change in what any of them resolve to.

## The finding

`call_compiled_function_named` and `call_compiled_function_named_inner` took the
callee's package and name as `&str`:

```rust
fn call_compiled_function_named_inner(..., fn_package: &str, fn_name: &str) {
    let fn_package_sym = Symbol::intern(fn_package);
    let fn_name_sym = Symbol::intern(fn_name);
```

and both symbols were then used, not the strings: they are what the routine
frame, the lazy `&?ROUTINE` code object, the callable-id key and the
declaring-package switch are built from. So the pair was interned once per call
purely because the signature asked for text — and `Symbol::intern` is a
thread-local, string-keyed hash probe that hashes and compares the whole key even
on a hit. One layer below, `find_compiled_function_inner` interned the same
callsite name a second time for its resolution-cache key.

Every caller already held the interned form:

- a callsite name is a **string constant**, and `CompiledCode::const_syms`
  (built at finalize time by [#7736](https://github.com/tokuhirom/mutsu/issues/7736))
  already carries its `Symbol` — `code.const_sym(name_idx)` is an indexed load;
- a resolved `FunctionDef` carries `package` and `name` as `Symbol` fields;
- the current package has an atomic `Symbol` mirror, `current_package_sym()`,
  which is one relaxed load.

The `&str` signature was also forcing *allocation*. Nine call sites built a
`String` they had no other use for, several of them twice over —
`self.current_package().to_string()` clones the package out from behind its
`RwLock` and then clones the clone — and three more called `.resolve()` on a
`Symbol` to get a `String` back out of the symbol table.

## The change

`fn_package` / `fn_name` are `Symbol` parameters, and `find_compiled_function`,
`find_compiled_function_memo` and `find_compiled_function_inner` take the
callsite name's symbol alongside the `&str` they still probe with. Inside the
callee the two `Symbol::intern` calls become `fn_name_sym.as_str()` — the
opposite direction, an indexed read out of the per-thread resolve cache with no
hashing — for the handful of places that still want text (`enhance_binding_error`,
the `&name` where-constraint lookup, the `LEAVE` routine key).

`exec_exec_call_op` and `exec_exec_call_pairs_op` read `code.const_sym(name_idx)`
once and hand that one symbol to the resolution probe, the native-function probe
and the named entry alike. `compile_and_call_function_def` passes `def.package`
and `def.name` straight through instead of `resolve()`-ing both into `String`s.
A `debug_assert_eq!(Symbol::lookup(name), Some(name_sym))` in the resolution
probe makes any future caller that pairs a name with the wrong symbol fail
loudly in CI's debug runs — `lookup` rather than `intern` so the assertion
neither grows the symbol table nor moves the counter the budget test below
reads.

## Measured

Deterministic counts, via `symbol::intern_calls()` — an exact per-thread counter
— read as the slope between a 100- and a 1100-iteration loop, so the figures do
not depend on machine speed or load:

| call shape | before | after |
| --- | --- | --- |
| `Test` assertion (`ok 1, "x"`) | 21.0 | 15.0 |
| `multi` call | 25.0 | 23.0 |
| `where`-constrained sub call | 63.0 | 61.0 |

The assertion shape moves furthest because it reaches the named entry twice —
`ok`, then `proclaim` — through `ExecCallPairs`, the opcode that exists for
exactly that call shape.

## What this does not do

The rest of [#7766](https://github.com/tokuhirom/mutsu/issues/7766) unit 2 is
untouched and the issue stays open for it: `user_method_overloads` /
`has_user_method` (item 2), `multi_arg_type_keys`' receiver-class name (item 3),
and the `resolve_function_multi_cached` / `fn_keys_for_base` /
`push_multi_dispatch_frame` layers (the rest of item 4). Those are `&str` APIs
fed by further `&str` APIs rather than by a caller that already holds a symbol,
so each wants its own `_sym` chain grown up the call graph.

## Regression cover

`tests/named_call_intern_budget.rs` pins the three shapes above as budgets, the
way `tests/regex_match_intern_budget.rs` pins the smartmatch path: all budgets
fail on the pre-change counts, so the saving cannot silently come back.

Each budget is calibrated twice, because a **debug** build interns strictly more
than a release one for reasons unrelated to this change: `current_source_file_sym`'s
own `debug_assert_eq!` calls `source_file_sym_by_walk`, which interns `?FILE` and
the whole declaring path on every routine entry and is compiled out in release —
the trap #7766's body warns about. Since `cargo test` is a debug build
everywhere here, the debug calibration is the one CI actually enforces, and it
was measured on both sides too (assertion 64.0 -> 58.0, multi 38.0 -> 36.0,
`where` 72.0 -> 70.0). A budget test that only knew the release numbers would
have failed the moment `make test` ran it.

`t/routines/signature/param-bind-symbol-keys.t` grows from 55 to 78 assertions
(all 78 verified against Rakudo first). It covers what this class of change gets
wrong — a *wrong name* reaching one of the consumers, rather than a slowdown:
`&?ROUTINE.name` / `.package`, a cross-package call resolving its declaring
package's `our` variable, an anonymous routine's `<anon>` sentinel, a `&`-sigil
code-variable call, a `where`-constrained call, proto/multi candidate selection,
`nextsame`, the `LEAVE` phaser's `"{package}::{name}"` routine key, and the
per-routine `$!` reset.
