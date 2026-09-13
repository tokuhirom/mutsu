# A closure call stops re-interning its own names

[#8302](https://github.com/tokuhirom/mutsu/issues/8302) measured invoking a
`Callable` at 47-62x rakudo and, more tellingly, **4.2x a named sub call in
mutsu itself** — an inverted direction, since rakudo makes a closure call
*cheaper* than a named one (it skips multi-dispatch). The issue was filed as
`todo:perf` precisely because nobody had established *what* the extra work was.

## What the profile said

callgrind on `-> $a { $a }` called 20,000 times (release, `MUTSU_JIT=off`), next
to the same loop over `sub named($a) { $a }`:

| | per call (Ir) |
| --- | --- |
| named sub, `call_compiled_function_positional_light_at` | 2,290 |
| closure, `call_compiled_closure` | 13,897 |

`Symbol::intern` and the thread-local memo it reads (`LocalKey::with`) together
accounted for ~2,290 instructions per closure call — about 16% of it, and the
largest single bucket that was not doing any of the call's actual work.

`symbol::intern_calls()` is an exact, load-independent counter, so the shape of
that waste can be stated without timing anything. Per invocation:

| | interns/call |
| --- | --- |
| named sub | 2 |
| bare block `{ $_ }` | 6 |
| pointy block `-> $a { $a }` | **18** |

Eight of the eighteen were the parameter name `"a"` alone, interned once each by
the binder, the readonly mark, the exit writeback's parameter-name set, and
three separate by-name `Env` probes.

## Why a pointy block, specifically

A pointy block reaches the signature binder with an **empty `param_defs`**: its
parameters survive only as the `String`s in `SubData::params`. Every mechanism
that had already been taught to work from pre-interned `Symbol`s —
`CompiledFunction::param_name_syms` (#7766), `CompiledCode::local_sym` (#7571),
`Env::get_for` (#7736) — keys off `param_defs` or off a `CompiledFunction`, so
the whole legacy path kept naming things with strings. The block a `.map` or a
`$callback(...)` invokes is exactly that shape.

## What changed

- **`SubData::param_name_syms()`** — a `ParamNameSyms` built once per code
  object, holding `params` interned index-parallel plus the set of every name
  the signature binds call-locally (`params`, each `param_defs[i].name`, and
  sub-signature names). A signature is immutable after construction, so the
  `FxHashSet` the exit writeback rebuilt on every call is now built once. Fed to
  the binder through a new
  `bind_function_args_values_with_legacy_syms` — the `param_name_syms`
  treatment for the legacy path — and to the `pointy_alias_param` readonly mark.
- **`SubData::source_file_sym()`** — the defining file interned once per code
  object rather than per call, mirroring `CompiledFunction::source_file_sym`.
  Used by the routine frame the call pushes and by `unit_of_source_sym`.
- **The two `cc.locals` loops** (seed from env on entry, flush back on exit) now
  probe and write with `cc.local_sym(i)`. The flush also stops allocating a
  fresh `String` per captured local per call. New `Env::contains_key_for` /
  `Env::insert_for` complete the `get_for` family.
- **Five `__mutsu_*` dispatcher-metadata probes** — `multi_dispatch_candidates`,
  `multi_dispatch_name`, `lookup_candidate_idx`, `lookup_class`,
  `lookup_method`, joining `callable_type` and `return_type` in
  `symbol::well_known`. `vm_call_on_value` and
  `sub_multi_method_dispatcher_name` run these on *every* indirect call on the
  way to the compiled closure path, and every one of them misses for an ordinary
  block.
- **The legacy `@_` insert** uses `wk::positional_slurpy()` instead of
  `"@_".to_string()`.

Three `debug_assert_eq!(sym, Symbol::intern(key))` guards became
`debug_assert_eq!(sym.as_str(), key)`. Interning is injective, so that is the
same assertion — but it no longer makes a debug-only `Symbol::intern` call that
the release build never makes, which had been hiding the improvement from the
counter the new test reads.

## Result

Same benchmark, same machine:

| | before | after |
| --- | --- | --- |
| interns per closure call | 18 | **2** |
| `Symbol::intern` calls, whole run | 285,161 | 65,165 |
| `Symbol::intern` self Ir | 7,894,289 | 937,946 |
| `LocalKey::with` Ir | 33,174,202 | 5,814,864 |
| `call_compiled_closure` Ir/call | 13,897 | **11,469** |
| whole benchmark Ir | 398,700,032 | **350,127,347** |

A closure call is 17.5% cheaper in retired instructions, and now interns one
name more than a named sub call rather than nine times as many.

## What is left, and why this does not close the 4x

The remaining gap is structural, not incidental, and it is worth naming
precisely because the profile now makes it unambiguous. A named sub takes
`call_compiled_function_positional_light_at`: parameters bind straight into
`self.locals` slots, the frame is a scoped overlay that is usually not even
forked, and the whole thing costs ~2,290 Ir. A closure takes
`call_compiled_closure_in_unit`, which still pays the general signature binder
(~4,650 Ir/call), an env create/drop pair, `push_caller_env`/`pop_call_frame`,
and the captured-env install. Nothing here changes that shape — the eligibility
predicate `is_positional_light_call_eligible` is written against
`CompiledFunction`, which a closure does not have.

Closing that is a separate, larger piece of work (a light path for closures, or
giving a closure literal a `CompiledFunction`-shaped signature description), and
is filed as its own issue rather than smuggled in here.

`tests/closure_call_intern_budget.rs` pins the result the same deterministic way
`tests/regex_match_intern_budget.rs` pins the smartmatch path: a budget on
`symbol::intern_calls()` per invocation, measured as the difference between two
loop lengths so it depends on neither machine speed nor load.
