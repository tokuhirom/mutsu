# SEGV in `spawn_callable_promise` under many concurrent `Promise.start`

`roast/S17-lowlevel/semaphore.t` (whitelisted, 2 tests, 4000 `Promise.start`
closures per subtest) SEGVs with 0 tests run at roughly **6-8% under CPU
contention**, in the jit-stress configuration. It is the S17 failure the prose
"known flaky" list in CLAUDE.md has always described in the abstract; this file
records the measurement and the actual crash site.

## Measured, and NOT a recent regression

Twelve concurrent copies on a 12-core box, release build,
`MUTSU_JIT=on MUTSU_JIT_THRESHOLD=2 MUTSU_FUDGE=1` (the exact `jit-stress` job
configuration), 48 runs per tree:

| tree                                        | SEGV / runs |
| ------------------------------------------- | ----------- |
| `17e4d38b5` (before the Digest PR #5822)     | 3 / 48      |
| `d0233c8ce` (after #5822 and #5834)          | 4 / 48      |

Statistically the same, so neither the `PostIncrementIndex` slot change in #5822
(which `@r[$i]++` in the second subtest exercises) nor the placeholder-scope
change in #5834 introduced it. Serially, and even at `-j12` through `prove`, it
does not reproduce at all: 24 consecutive clean runs. It needs *many independent
processes* competing for cores. `scripts/flake-repro.sh` is the intended tool;
the ad-hoc driver used here was 12 background copies per round, 4 rounds.

## The crash site

`MUTSU_CRASH_DIR` catches it (`tmp/crash/<pid>.txt`):

    signal: 11 (SIGSEGV)
    si_code: 1                      # SEGV_MAPERR - address not mapped
    fault-addr: 0x000075762bfff9c8
    thread: mutsu
    ...
      3: ___pthread_detach at ./nptl/pthread_detach.c:36:6
      4: core::ptr::drop_in_place<std::thread::join_handle::JoinHandle<()>>
      5: mutsu::runtime::builtins_system::…::spawn_callable_promise
      6: mutsu::runtime::methods_promise_class::…::dispatch_promise_start
      7: mutsu::runtime::methods_dispatch_match::…::dispatch_method_by_name_1
      8: mutsu::runtime::methods_call_dispatch::…::call_method_with_values
      9: mutsu::runtime::methods_mut_dispatch::…::call_method_mut_with_values
     10: mutsu::vm::vm_call_method_compiled_mut::…::try_compiled_method_mut_or_interpret_sym
     11: mutsu::vm::vm_call_method_mut_ops::…::exec_call_method_mut_op_impl
     13: mutsu::vm::vm_jit_helpers::call_method_mut

The faulting address sits just below a page boundary (`…fff9c8`) with
`SEGV_MAPERR`, which is the signature of a **guard-page hit**, i.e. a stack
overflow — not a corrupt `JoinHandle`. `pthread_detach` is simply the innermost
frame when the already-deep VM stack runs out: dropping the `JoinHandle` that
`spawn_callable_promise` returns detaches the thread, and that call is made from
inside a nested VM dispatch chain (`vm_jit_helpers::call_method_mut` →
`exec_call_method_mut_op` → … → `dispatch_promise_start`), on the VM's own
spawned thread rather than on the main thread.

## Why this is a deep item, not a ticket

Two things have to be settled together:

1. **How much stack a VM thread gets, and who decides.** The main VM thread is
   spawned with an explicit stack size; the threads `Promise.start` creates
   inherit whatever the runtime default is. If 4000 outstanding promises can
   push a dispatch chain over the limit, the fix is not "raise the number" —
   it is deciding where the VM's recursion budget is declared and enforcing it
   uniformly for every thread the interpreter spawns (see also
   `roast/integration/deep-recursion-initing-native-array.t`, which overflows in
   a *debug* build for the same underlying reason).
2. **Whether `spawn_callable_promise` should hold a `JoinHandle` at all.** The
   crash is in the *drop* of a handle the promise machinery does not join. A
   design in which the spawned thread's completion is observed through the
   `Promise` (and the handle is detached at a shallow stack depth, or never
   created) removes this frame from the deep path entirely.

Until then the test stays whitelisted and un-quarantined: quarantining a SEGV
would hide a real memory-safety-adjacent defect, and a single CI occurrence
passes on re-run. Do **not** add it to `flaky-tests.txt` without revisiting this
decision.
