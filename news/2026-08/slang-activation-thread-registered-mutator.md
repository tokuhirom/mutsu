# Slang activation now spawns a registered, large-stack GC mutator

`src/runtime/slang_activation.rs`'s `run_slang_activation` was the only place
in non-test `src/` that ran user Raku code on a thread spawned with raw
`std::thread::Builder::spawn` instead of going through
`builtins_system::spawn_user_thread`. It carried three latent defects, all
fixed by routing it through the standard wrapper:

1. **Unregistered GC mutator.** GC registration happens only via
   `gc_register_main_thread()` and `spawn_registered_thread` (the shared
   implementation behind `spawn_user_thread`/`spawn_gc_helper_thread`); a raw
   `Builder::spawn` reached neither. This thread built an entire
   `Interpreter`, loaded a slang module, and ran its mainline — creating,
   cloning, and dropping `Gc` values throughout — while invisible to the
   collector. Per `gc::stw`'s documented rule ("only registered threads count
   toward quiescence"), an unregistered thread's `Gc` mutation landing
   mid-scan can corrupt Bacon-Rajan trial deletion, the same bug class as the
   2026-07-16 GC survivor Purple-color violation (`#4589`).
2. **Bare `join()` outside `block_quiescent`.** Every other blocking join in
   the runtime (`native_proc_async.rs`, `supply_transform.rs`,
   `worker_pool.rs`, `methods_collection_ops/socket_thread.rs`) wraps the wait
   in `gc::block_quiescent` so the waiting (registered) thread does not starve
   stop-the-world. This site's parent blocked in a bare `handle.join()`, which
   could deadlock STW against a collection requested during a slang module
   load.
3. **Default (~2 MiB) stack for arbitrary user code.** Every other
   `spawn_user_thread` worker gets the 256 MiB stack reserved specifically
   because "the default ~2 MiB thread stack overflows on deep VM recursion".
   This thread ran a module's mainline — arbitrary user code, including
   grammar recursion — on the default stack, so a non-trivial slang module
   grammar could abort the process with a stack overflow.

## Fix

`run_slang_activation` now spawns via
`builtins_system::spawn_user_thread(ACTIVATION_THREAD_NAME, ...)` (registered
mutator, 256 MiB stack) and joins via `gc::block_quiescent(|| handle.join())`.

The thread **name** stayed load-bearing: `parser::stmt::simple::slang_use`'s
`maybe_activate_slang_use` reads `std::thread::current().name()` to refuse
recursive slang activation from inside an already-activating sub-interpreter.
`spawn_thread` passes `name` straight to `Builder::name`, so this kept
working automatically, but it is no longer just trusted — a new unit test
(`slang_activation::tests::activation_thread_reports_the_name_the_recursion_guard_checks`)
spawns via the real `spawn_user_thread` path and asserts the spawned thread's
own `std::thread::current().name()` is `ACTIVATION_THREAD_NAME`, so a future
refactor that stops threading the name through cannot silently disable the
recursion guard.

**Spawn-failure behavior change (deliberate):** `spawn_user_thread` bottoms
out in `thread_compat::spawn_thread`, which does
`.expect("failed to spawn worker thread")` on OS spawn failure, whereas the
old code returned a recoverable `Err(...)`. This PR accepts that behavior
change rather than adding a `try_` variant of `spawn_registered_thread`: every
other call site of `spawn_user_thread`/`spawn_gc_helper_thread`
(`worker_pool.rs`, `supply_promise.rs`, `methods_collection_ops/socket_thread.rs`)
already accepts the panic-on-spawn-failure convention, so a `try_` variant
would exist for exactly one caller. OS thread-spawn failure (resource
exhaustion) is unrecoverable in practice, and this keeps the slang activation
path consistent with the rest of the runtime instead of introducing a new,
single-caller error-handling shape.

## Verification

`t/slang-piersing-activation.t` and `t/slang-tuxic-activation.t` pass under
`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`, both serially
and under `prove -j4` release-build load, with zero `VERIFY FAIL` output
across multiple runs.
