# `Digest::RIPEMD` is 11x raku — a `start` per compression block

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
