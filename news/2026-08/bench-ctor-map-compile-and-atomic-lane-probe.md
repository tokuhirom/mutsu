# bench-ctor: the rw map loop re-ran the whole compiler on every call

`bench-ctor` — the heavy-constructor benchmark modelled on `Zef::Distribution`
(21 attributes, a user `method new(*%_)` delegating to `self.bless`, and `TWEAK`
submethods at two MRO levels) — had drifted to **1.31x rakudo** in the bench CI.
Decomposing the benchmark into the four features it layers on top of a plain
`.new` located the cost precisely.

Interleaved same-session A/B, release builds of this change and of its revert,
`taskset -c 2`, best of 9, 5000 constructions:

| variant | before | after | raku |
|---|---|---|---|
| A: 21-attribute class, plain `.new` | 0.0551 | 0.0562 | 0.2208 |
| B: + a parent class with `TWEAK` | 0.0737 | 0.0755 | 0.2367 |
| C: + `method new(*%_) { self.bless(\|%_, :meta(%_)) }` | 0.1365 | 0.1354 | 0.2561 |
| D: + child `TWEAK` with attributive params and `--> Nil` | 0.1545 | 0.1544 | 0.2691 |
| full: + `@!resources = @!resources.map(*.flat)` | **0.2515** | **0.2191** | 0.2904 |

The whole win is the last row: one `.map` over an **empty** attribute array.
Everything above it is unchanged (the ±2% spread on A/B/C is binary-layout
noise). `perf stat`: 2.172G → 1.951G instructions for the run, i.e. **434k →
390k per construction (−10.1%)**; wall clock **−12.9%**.

The local raku ratio is deliberately not quoted as the headline: this box's
rakudo v2026.06 is much faster than the 4-core CI runner's pinned build, so the
local ratio (well under 1.0 both before and after) is not comparable to the CI
series. `bench-history.tsv` on `bench-data` is the authority for that number.

## Root cause: `eval_map_over_items_rw` never got the compile cache

`compile_loop_block_cached` (added with the map/grep compile cache, #6710) keys a
compiled block on its origin `CompiledCode` plus whether a routine is on the
stack, so a `.map` whose block is the same closure literal compiles once for the
life of the program. `eval_map_over_items`, `eval_first_over_items` and the rw
grep loop all go through it — but `eval_map_over_items_rw`, the path every
`@array.map(...)` and `@!attr.map(...)` takes, still called
`Compiler::new().compile(&normalized_body)` directly. A `rust-gdb` breakpoint on
`CompiledCode::add_constant` caught it red-handed: the backtrace ran
`add_constant` → `compile_expr_var` → `compile_expr` → `compile_unit` →
`Compiler::compile` → `eval_map_over_items_rw`, on every single iteration.
`MUTSU_VM_STATS=1` over a 100000-call loop counted **300017 constant-pool
additions at runtime**; with the cache it is **20**. On `bench-ctor` itself:
15044 → 44.

Going through the cache also brings this path in line with its siblings on
`lexically_in_routine`, which is what decides whether a typed `my` inside the
block is frame-scoped.

## Second cause: an empty `.map` paid the whole setup for nothing

Even with the compile cached, an empty input still walked the block's captured
env to build the save/restore key list, mirrored those keys into the running env,
built the nested-register frame and tore it all down again — to run the block
zero times. Both `Sub` branches of `eval_map_over_items_rw` reach exactly
`(Value::array(vec![]), false)` for an empty input, so it now returns that
directly. As a side effect an inner empty map can no longer remove an enclosing
map's `topic_key` mid-iteration.

`@!resources.map(*.flat)` inside a `TWEAK` — an empty attribute array mapped in
place — is a shape real code uses, not a benchmark artefact. The isolated micro
(5000 such calls inside a method) went **0.0985s → 0.0789s, −20%**.

## Third cause: every `@`/`%` read probed the atomic container lane

Concurrent `.push` / element-assign publish an authoritative copy of a container
under a `__mutsu_atomic_arr::` / `__mutsu_atomic_hash::` key in the cross-thread
store (#4167), and `get_env_with_main_alias_inner` preferred that copy. To do so
it built the lane key with `format!` and walked the store chain — on **every**
array or hash variable read, in every program, whether or not any atomic
container op had ever run. That is one heap allocation plus a chained hash lookup
per read; `format!` showed up in the profile of an empty-map loop, and `gdb`
placed it inside `get_env_with_main_alias_inner`.

`runtime::shared_store` now carries a monotonic `atomic_lane_entries_exist()`
flag, armed whenever a lane entry is inserted (the four `SharedStore` mutators)
or a caller reaches for `atomic_lane_scope` to insert one through `own_map()`
directly. A lane can only be *read* after it has been *written*, and the write
arms the flag, so concurrent behaviour is unchanged. This is the `@`/`%` twin of
the atomic *scalar* read gate already pinned by `t/atomic-read-gate.t`, and
follows the same pattern as `CheckReadOnly`'s `bound_marker_possible()` gate.

Its wall-clock effect on this benchmark is below noise (−1.4% instructions on the
`.map`-free variant); it is in here because it removes a per-read heap allocation
on the hottest variable-access path in the interpreter, not for the bench number.

## Why four earlier profiling rounds on this ticket missed it

`todo/perf/bench-ctor-construction-parity.md` rounds 2-4 each concluded "flat
profile, no dominant function; the cost is spread across malloc / NaN-box /
hashing". A per-call compile is exactly what that conclusion looks like from a
flat profile: it spreads across `compile_expr`, `compile_unit`, `add_constant`,
`ws_inner_with_bol` and the allocation those do. The cheap oracle, recorded in
the ticket's round-5 update: on a steady-state loop, `MUTSU_VM_STATS`'s
`const-pool: add_constant=` must not grow with the iteration count.

## Pins

`t/map-rw-empty-and-compile-cache.t` (17 assertions: empty input, write-back,
`state` scoping, `return`-from-the-enclosing-routine, Slip flattening,
multi-arity blocks, typed `my` in the block) and
`t/atomic-container-lane-read-gate.t` (12 assertions: ordinary reads with the
gate off and on, concurrent array pushes and hash element writes). Both are green
under real `raku`.
