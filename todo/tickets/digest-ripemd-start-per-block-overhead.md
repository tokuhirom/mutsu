# `Digest::RIPEMD` is 11x raku — a `start` per compression block

> **Status update 6 (2026-08-25, dispatch-probe caches — -13.4%
> instructions on the gate proxy; spawn-shape bench re-verified faster
> than raku):** re-measured the headline numbers on current `main`
> first: the spawn-shape microbench (`for ^2000 { await map -> $k {
> start { $k * 2 } }, 1, 2 }`) runs **0.19s in mutsu vs 0.41s in
> raku** — the per-`start` lever is confirmed closed (status update 1's
> campaign held), and the "~17x raku" claim at the bottom of this file
> is historical. The full `t/ripemd.t` measured **156.6s** (release,
> idle box, 9/9 pass) against the 120s gate budget.
>
> A fresh profile (release-with-debuginfo, `rmd160("a" x 50_000)`)
> found a new nameable cluster the earlier flat profiles lumped into
> "malloc + TLS + memcmp": the **per-call bare-name dispatch probes**.
> Every `CallFunc` dispatch ran `Interpreter::has_proto` up to 3 times
> (plus `has_declared_function` / `has_multi_function` on the fallback
> leg), and each probe allocated the `bare_name_packages()`
> `Vec<String>` and two `format!("{pkg}::{name}")` strings per
> candidate package — `has_multi_function` additionally resolved EVERY
> registry function key to a `String` per call. gdb breakpoints on
> `alloc::fmt::format::format_inner` mid-run attributed the hot
> formatter traffic to exactly `Registry::has_proto` /
> `has_declared_function` (via `RADIX_LIST` and the round helpers).
>
> Fix (same shape as the existing `multi_candidates_cache`):
> generation-checked memos for all three probes, keyed by
> `(current_package, innermost lexical_package, name)` symbols — the
> exact inputs `bare_name_packages()` derives the search list from, so
> a hit can never answer for the wrong package scope.
> `has_declared_function`/`has_multi_function` ride the established
> `fn_resolve_gen`; `has_proto` gets a new `proto_gen` on a now-private
> `Registry::proto_subs` whose every mutation flows through
> gen-bumping accessors (compiler-enforced completeness, per the
> narrow-audit-scope lesson).
>
> **Measured** (interleaved A/B, P-core-pinned `perf stat`, instruction
> counts are run-to-run stable to 4 digits): the 100k gate proxy drops
> **215.3B -> 186.4B core instructions (-13.4%)**, cycles -5..-10%,
> pinned wall 24.3/23.3s -> 22.3/21.8s; the spawn microbench drops
> 1.55B -> 1.34B instructions (-13.7%). Wall-clock deltas on this
> thermally-throttled hybrid laptop are noisy (an unpinned run right
> after a long build read 21s vs the 12.8s cool-box baseline — pure
> thermal artifact, caught by the pinned interleave); the instruction
> counts are the trustworthy figure. A back-to-back full `t/ripemd.t`
> A/B under equal (hot-throttled) conditions measured **223.2s wall /
> 349 user-s (old) vs 211.8s wall / 328 user-s (new)** — about -5..-6%
> end-to-end (the non-dispatch half of the profile dilutes the -13.4%).
> Scaled to the session's cool-box baseline (156.6s) that is ~148s —
> still over the 120s hard budget, so the file stays un-whitelisted and
> this ticket stays open.
>
> Post-fix profile has no new dominant item: thread-local `LocalKey`
> (symbol TLS) ~6%, malloc/free ~12%, `memcmp` ~3.9%, nanbox
> `payload_op`/`gc_op` ~7%, `call_compiled_closure_with_topic` 2.7%.
> The update-4 inventory items (the `cc.has_calls` exit-path writeback
> scan, `GetGlobal` env reads) remain the next levers.

> **Status update 5 (2026-08-20, map/grep/`.first` compile-cache lever
> executed — general win confirmed, `t/ripemd.t` gate itself flat):** update
> 4's closing note flagged "the `resolution_map_grep` map carrier still
> recompiles per call/per map invocation" as an untried lever with the same
> shape as the `reduce_items` compiled-first fix. Implemented it: the map/
> grep/`.first` inline-loop fast paths (`resolution_map_grep.rs`,
> `resolution_map_grep_rw.rs`) used to run `compiler.compile()` on the
> callback block's (tail-normalized) body on every `.map()`/`.grep()`/
> `.first()` CALL, even when that call site's closure literal is the exact
> same source occurrence as a previous call (e.g. a block declared inside an
> outer loop, called fresh each iteration with a brand-new `SubData`). A new
> `Interpreter::compile_loop_block_cached` reuses a cached compile keyed by
> the closure's pre-existing `compiled_code` `Arc` pointer identity (already
> shared across every instantiation of the same source closure literal —
> baked once into the enclosing scope's `closure_compiled_codes` at that
> scope's own compile time — so the pointer is a free, stable cache key,
> **not** a new id-minting scheme).
>
> **Correctness pitfall caught during development, now fixed:** an early
> version keyed the cache on the bare pointer *address* (`usize`) without
> retaining the Arc, which is unsound whenever the source Arc's only owner is
> a short-lived `SubData` (true for a dynamically-built `EVAL`/RakuAST
> closure, never retained in any `closure_compiled_codes` table) — once
> dropped, a later unrelated `CompiledCode` allocation can reuse the same
> address and collide with a stale cache entry. `t/rakuast-eval-block-arg.t`
> caught this immediately (chained `.map().grep()` on one EVAL'd line
> returned the wrong predicate's result). Fixed by making the cache key
> (`MapGrepCacheKey`) hold a clone of the Arc itself (custom pointer-identity
> `Hash`/`Eq`), so the key keeps the source `CompiledCode` alive for as long
> as the cache entry does. Pinned by `t/map-grep-first-compile-cache.t`
> (loop-redeclared blocks for map/grep/first, plus a repeated-distinct-EVAL
> stress case for the pointer-reuse scenario specifically).
>
> **Measured, isolated general win** (release, synthetic "closure declared
> inside an outer loop, few items per call" benchmarks — the exact shape this
> lever targets): `.map()` ~2.18s → ~1.71s (~22% faster), `.grep()` ~2.93s →
> ~2.43s (~17%), `.first()` ~1.42s → ~1.03s (~28%) over 200k outer
> iterations. `cargo test`, the full `t/` suite (30223 tests), and the
> map/grep/reduce/first roast files all pass with no behavior change.
>
> **`t/ripemd.t`'s own gate is flat**, though (release, same machine,
> back-to-back A/B: ~139s before, ~146s after — within run-to-run noise, no
> real change either way). This is consistent with update 4's own flat
> profile finding "no single dominant item left" — the outer
> `map -> [&f, $r, @K, $s] { start {...} }, zip(...)` this lever targets is
> evidently not RIPEMD's dominant cost; the malloc/free, thread-local symbol
> caches, and `call_compiled_closure_with_topic` items from that profile
> remain unaddressed. Landing this anyway as a genuine, general, verified
> interpreter improvement (independent of whether it moves any one ticket's
> needle) — not re-attempting `t/ripemd.t` specifically without a fresh
> profile pointing at a new dominant item.

> **Status update 4 (2026-08-05, closure-setup + reduce compiled-first —
> gate 28s → 12s, `t/ripemd.t` 295s → 119s):** two levers executed.
> Slice 1 (#5941) landed the top closure-call setup allocations from the
> update-3 inventory: `&?BLOCK`/block_stack now reuse the caller's
> `Gc<SubData>` (the signature takes `&Gc<SubData>`), the
> `"self"`/`"&?BLOCK"`/`"__mutsu_callable_id"`/`"!"`/`"_"` env inserts
> are symbol-keyed, and `sanitize_call_args_owned` passes the caller's
> args `Vec` through untouched when no callsite marker is present.
> Closure-call microbench 4.30s → 3.7s; gate ~flat (29.6 → 28.3) — the
> setup malloc was real but not dominant. The dominant cost turned out
> to be one level up: **`reduce` dispatched every step through
> `call_sub_value`, whose body execution is the `eval_block_value` AST
> carrier — a full recompile of the 80-round reduce lambda (including
> its `BEGIN`-array's five anon subs) per step, per task** (gdb:
> `compile_routine_closure_body` fired 300+ times in a 2k run; perf put
> ~10% of the run in the compiler + its malloc traffic). #5942 makes
> `reduce_items` compiled-first: a Sub with `compiled_code` /
> `compiled_routine` dispatches through `vm_call_on_value` (now
> `pub(crate)`); AST-only Subs keep the carrier. Gate: **28s → 12.0s**;
> full upstream `t/ripemd.t`: **295s → 119s** (9/9). Still NOT
> whitelisted: the battery gate is a hard `timeout 120` per file and
> 119s local leaves no margin for slower CI runners — one more ~20%
> lever is needed. The post-#5942 flat profile (release, 20k input) has
> no single dominant item left: `nanbox::gc_op` (refcount) 4.7%,
> thread-local symbol caches ~5.5%, `memcmp` 3.9%,
> `call_compiled_closure_with_topic` 3.1%, malloc+free ~12%,
> `Symbol::intern` 2.5%. Remaining inventory levers still apply — the
> `cc.has_calls` exit-path writeback scan (ADR-0018 narrowing), the
> `GetGlobal` env reads — and `produce` / the `resolution_map_grep` map
> carrier still recompile per call/per map invocation and could get the
> same compiled-first treatment.

> **Status update 3 (2026-08-05, Blob AT-POS lever executed — gate FLAT
> again; fast path is a real win elsewhere):** the `Index`/`AT-POS` lever
> from update 2 landed (#5939): a dedicated `(Instance, Int)` arm in
> `exec_index_op_with_positional` decodes one Buf/Blob element in place
> via `value_buf::buf_elem_at`, bypassing the AT-POS dispatch chain (the
> per-access parametrized `class_mro("Blob[uint32]")` resolve) and
> `decode_elems`' whole-buffer `Vec<Value>` per read (O(N^2) for a loop).
> A 1M-iteration blob32 element-read loop drops 12.0s -> 0.79s (~15x),
> but the rmd160 gate is **flat** (~29.6s at 100k input, release): the
> RIPEMD hot loop's subscripts are evidently not the dominant cost — the
> per-round **closure-call setup malloc** (~20% of the flat profile) is
> the next lever, then the `GetGlobal` env reads. Per-call allocation
> inventory of `call_compiled_closure_with_topic`
> (vm_closure_dispatch.rs:121, surveyed 2026-08-05), largest first:
>
> - **`Gc::new(SubData {...})` for `&?BLOCK`** (~:416) — clones params /
>   env / captures / compiled_code per call, built even when the body
>   never mentions `&?BLOCK`. Single largest item; make it lazy or gate
>   on a compile-time "body mentions &?BLOCK" flag.
> - **String-keyed `Env::insert`** of `"self"` / `"&?BLOCK"` /
>   `"__mutsu_callable_id"` / `"!"` — a `String` alloc + `Symbol::intern`
>   each, every call (:311/:439/:477/:564); use interned-symbol inserts.
> - **Three `Symbol::resolve()` `String` allocs** per call for
>   package/name (:461-475, :564, :667) — one just to test emptiness.
> - `sanitize_call_args` rebuilds the args `Vec` the caller already owns
>   (:130); `locals = vec![NIL; n]` (:602); `free_at_entry` snapshot
>   clone of every free var (:642).
> - Exit path when `cc.has_calls` (i.e. any non-leaf closure): four hash
>   sets + a full-env writeback scan (:1071-:1170) — the ADR-0018
>   narrowing target.
> - `Env::scoped_child` itself is already allocation-free steady-state;
>   the captured-env merge loop's first `cow_mut` write un-shares the
>   overlay map per call (:268-308 + env.rs:593).
>
> Side find, fixed forward: jit-stress on #5937 caught the documented
> TODO(J2) gap — a Rust panic inside a JIT shim aborted the process at
> the `extern "C"` edge (deterministic SEGV on
> t/hyper-race-panic-boundary.t once shared OTF bodies let worker
> closures reach the compile threshold). Fixed in #5938: shims run under
> a `panic_boundary` (`catch_unwind` -> parked payload ->
> `JIT_STATUS_PANIC` -> `resume_unwind` in `try_enter*`).

> **Status update 2 (2026-08-05, JIT bitwise lever executed — gate FLAT):**
> the Tier A bitwise coverage landed together with four deeper fixes it
> surfaced (per-task registry COW clones from `class_mro`, per-task OTF
> recompiles resetting JIT state, no JIT entry on the closure dispatch path,
> a per-call test-assertion full resolve) — see
> `news/2026-08/jit-bitwise-tier-a-coverage.md`. The hot chunks now compile
> (`bailouts=0`, `compiles` = one per distinct body process-wide), and
> `t/ripemd.t` is **unchanged**: 295.3s → 299.0s local (9/9 pass; JIT on/off
> A/B at `rmd160("a" x 20_000)` is flat ~6.0s both). The "JIT unlocks the
> loop" hypothesis is refuted: Tier A subroutine threading removes only
> dispatch overhead, and the per-round cost lives inside the opcode helpers.
> The measured flat profile (release `--profile profiling`) says the next
> levers are:
>
> - **~20% malloc/free**: per-round closure-call setup in
>   `call_compiled_closure_with_topic` — scoped env overlay creation, the
>   captured-env merge loop, args Vec — paid 80× per block × 31k tasks. A
>   leaf-closure fast path (no overlay when the body provably touches only
>   locals) or Tier B-style inlined param binding is the shape of the fix.
> - **`Index`/`AT-POS` dispatch**: each `@words[...]`/`$A[...]` element read
>   routes through `try_user_io_handle_method`, which probes
>   `class_mro("Blob[uint32]")` per access (now read-only, but still a
>   resolve + Arc build per op). A Blob-positional fast path in
>   `exec_index_op_with_positional` would bypass method dispatch entirely.
> - **`GetGlobal` env reads** (97k per 2k-input repro): the captured free
>   vars (`@words`, `@K`, `$r`, `$s`, `&f`) are env-resolved per opcode.
>
> Spawn-side work stays closed (status update 1 below).

> **Status update 1 (2026-08-05, end of the clone-slimming campaign):** the
> spawn-overhead lever is DONE — slices 0-5A of
> `docs/per-task-clone-slimming.md` merged (#5928/#5929/#5930/#5931/#5932/
> #5933; slice 5 step B retired by measurement, see the plan doc), and
> slice 6 (#5934) is on auto-merge. The spawn-shape bench below went 5.53s → **0.19s**
> (now below raku's 0.33s), and `t/ripemd.t` went ~513s → **295.3s** (9/9
> pass) — still over the 120s budget.

> **Implementation plan:** `docs/per-task-clone-slimming.md` (2026-08-05) —
> slice-by-slice design with measured baselines. The ADR-0020 worker pool
> landed first (all slices merged 2026-08-05); the remaining lever is the
> per-task `clone_for_thread` payload, dominated by the `Registry` deep
> clone + drop (perf: `_int_free` 25.7% + `drop_in_place<ClassDef>` on the
> worker side).

`rmd160` is correct on every RFC vector, but the bundled `Digest` battery's
`t/ripemd.t` is the one upstream file that cannot be whitelisted: it takes
~513s against raku's ~46s, over the batteries gate's 120s per-file budget
(`docs/batteries/digest.md`). The cost is entirely in the last vector,
`'a' x 1_000_000`.

## Where it goes

`modules/Digest/lib/Digest/RIPEMD.rakumod` runs the two halves of each
compression round concurrently:

```raku
blob32.new: [Z+] map {$_[[^5].rotate(++$)]}, $h, |await
  map -> [&f, $r, @K, $s] {
    start { reduce -> $A, $j { … }, $h, |^80 }
  }, …
```

A 1 MB message is 15625 blocks, so the run spawns ~31k `start` tasks, each
doing an 80-round reduce. That is ~33ms per block; raku manages ~3ms.

Two candidate costs were measured (release build, 2026-08-05):

1. **Per-`start` overhead is the confirmed lever — ~17x raku.**

   ```raku
   for ^2000 { await map -> $k { start { $k * 2 } }, 1, 2 }
   # mutsu 5.53s   raku 0.332s
   ```

   That is the exact shape `rmd160` runs per block. At 15625 blocks it is ~43s
   of pure spawn/await overhead in mutsu against ~2.6s in raku — a large slice
   of the gap, though not all of it, so there is per-round interpreter cost on
   top.

2. **The `shared_vars_active` latch is NOT the problem.** The first `start` in a
   process turns on the name-keyed shared-variable lane permanently
   (`runtime_thread.rs`), which looked like it might degrade every later
   lexical access. Measured: a 200k-iteration scalar+array-push loop runs
   0.576s before a `start` and 0.402s after it. No degradation — do not spend
   time here.

   (That latch is still worth understanding: it produced one silent correctness
   bug, the native-array push path skipping element typing while it was on —
   `news/2026-08/native-array-push-after-a-start.md`.)

## Repro

```sh
cargo build --release
D=tmp/libdigest-clone   # or any checkout of grondilu/libdigest-raku
timeout 900 ./target/release/mutsu -I modules/Digest/lib $D/t/ripemd.t
# 9/9 correct, ~513s     (raku -I lib t/ripemd.t: ~46s)
```

A smaller handle on the same path:

```sh
./target/release/mutsu -e 'use Digest::RIPEMD; rmd160("a" x 100_000)'
```

## Not a blocker

Every digest in the dist is correct, and the other three upstream files are
whitelisted and fast (2.2s / 1.5s / 5.9s). This ticket is about closing the
throughput gap so `ripemd.t` can join them.
