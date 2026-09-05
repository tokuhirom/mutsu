# `nqp::` ops and name-keyed dispatch stop walking the whole function registry

The vendored upstream `Test.rakumod` (`MUTSU_REAL_TEST=1`) was ~40x slower per
assertion than rakudo's own, and that gap — not any behavioural difference — is
what still blocks making it the default provider
(`todo/deep/vendor-real-test-module.md`). Profiling the real module on a
2000-assertion file found the cost was not in `Test.rakumod` at all: it was in
mutsu's name resolution, which walked the entire registry several times per
call. Four fixes, all general-purpose, halve the per-assertion cost.

## `nqp::` ops are dispatched at the call site

`proclaim` — the routine behind every single assertion in the real module —
runs `nqp::join`/`nqp::split` four times, and `ok` reads `nqp::time` twice.
Every one of those calls used to walk the complete dispatch chain (light-call
caches, proto/multi candidate scans, the native-function tables) and then the
interpreter's builtin fallback chain, only to be recognized by a
`strip_prefix("nqp::")` at the very bottom. Measured on 2000 assertions: three
by-name `resolve_function_with_types` calls plus two O(registry)
`has_multi_candidates` scans **per op**, and 83.3% of all function-call opcodes
falling back to the interpreter.

The `nqp::` namespace is reserved — no user routine can be declared in it — so
none of that walk can ever change the answer. `dispatch_nqp_op` is now the
single entry point (`src/runtime/nqp_ops_builtin.rs`): the interpreter-coupled
ops that used to be arms of `call_function`'s builtin `match`
(`atkey`/`atpos`/`ordat`/`sha1`/`gethostname`/`bindattr`/`decont`/`unbox_i`/
`box_i`/`setelems`) moved into it, and it falls through to the pure value table
in `nqp_ops.rs`. Three call sites short-circuit into it: the VM's `CallFunc`
opcode, the VM's bareword term opcode (`nqp::time` is written without
parentheses throughout `Test.rakumod`), and `Interpreter::call_function`.

The VM's short-circuit sits *after* the call's argument normalization, not
before it: `nqp::eqaddr(Int, Int)` depends on the `VarRef` unwrapping that
`normalize_call_args_for_target` applies to an unregistered name, and skipping
it made `eqaddr` answer 0 for identical type objects. `t/nqp-process-ops.t`
pins that.

Interpreter fallbacks on that file went 83.3% -> 0%, and by-name resolutions
50043 -> 12010.

## The registry has a base-name index

`resolve_function_with_types` gathered candidates by iterating the *whole*
functions map — up to four separate passes per resolution, each formatting a
prefix `String` per package and copying every key into a fresh `String` via
`Symbol::resolve()` just to test a prefix. With `Test.rakumod` loaded that is a
few thousand keys walked several times per assertion.

Every key a name-keyed dispatch can match (`Pkg::name`, `Pkg::name/<arity>`,
`Pkg::name/<arity>:<types>`, `…__m<n>`) reduces to the same base name under
`function_key_base_name`, so `Interpreter::fn_keys_for_base` now indexes the
map by that base name and every gather iterates the handful of keys it returns.
The index is filled lazily and dropped wholesale when `fn_resolve_gen` moves —
the same generation contract the surrounding resolution caches already stand
on — and it replaces the `fn_base_name_cache` bool memo, whose debug-only
staleness audit it inherits and strengthens (the audit now compares the whole
key list, not just whether the base is present, at the same one-scan-per-
resolution debug cost).

The remaining full-map scans on this path (`Registry::has_multi_candidates`,
`has_multi_function`, `resolve_all_multi_candidates`) now test prefixes against
`Symbol::as_str` (a `&'static str` out of the interner) instead of
`Symbol::resolve` (a fresh `String` per key).

## A lone `multi` candidate is cacheable

`func_multi_dispatch_type_cacheable` gated the sound multi-resolution cache on
having at least *two* candidates, on the reasoning that a single candidate is
what the name-keyed light-call caches already handle. That is wrong for a lone
`multi sub`: it is registered only under its arity key (`GLOBAL::ok/2`), so the
resolver's exact-name lookup misses and every call pays the full candidate
walk. That is exactly the shape of rakudo's `multi sub ok(Mu $cond, $desc = '')`.
The gate is now "at least one"; what makes a cached winner sound is the
value-dependency analysis above it, which does not care how many candidates
there are. `resolve_function_with_alias` and `find_compiled_function_inner`
both consult the cache now.

## The native-provider probe is only asked when it can win

`Interpreter::exec_call` asked `user_test_decl_beats_native` — a full
`resolve_function_with_types` plus `args_match_param_types` over the routine's
candidates — before checking whether the native TAP provider was even eligible.
Under `MUTSU_REAL_TEST=1` it never is, so the probe's answer had no consumer.
It is now asked second.

## Result

Release build, 2000 `ok 1, "x"` assertions under `MUTSU_REAL_TEST=1`, and the
callgrind instruction count for the same run:

| | wall clock | instructions |
| --- | --- | --- |
| before | 1.310 s | 6.22 G |
| after | 0.650 s | ~2.5 G |

Per assertion that is 0.64 ms -> 0.31 ms. The native provider answers the same
file in 0.017 s, so the vendored module is still the slower of the two and the
`S03-buf/{write-int,read-write-bits}.t` timeout class in
`todo/deep/vendor-real-test-module.md` is narrowed rather than closed; the
ticket carries the re-measured numbers and what is left.
