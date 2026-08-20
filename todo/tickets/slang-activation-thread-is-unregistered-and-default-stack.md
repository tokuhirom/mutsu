# The slang-activation thread is an unregistered GC mutator on a default-size stack

`src/runtime/slang_activation.rs:57-72` is the only place in non-test `src/` that
spawns a thread which runs user Raku code without going through
`builtins_system::spawn_registered_thread`. It breaks two invariants the rest of
the runtime holds uniformly.

```rust
let handle = std::thread::Builder::new()
    .name(ACTIVATION_THREAD_NAME.to_string())
    .spawn(move || -> Result<Vec<String>, String> {
        let mut interp = Interpreter::new();
        for path in lib_paths { interp.add_lib_path(path); }
        interp.env.insert("*LANG".to_string(), comp_lang_instance());
        interp.use_module(&module).map_err(|e| e.message.clone())?;
        Ok(std::mem::take(&mut interp.defined_slang_rules))
    })
    .map_err(|e| format!("could not spawn slang activation thread: {e}"))?;
handle
    .join()
    .map_err(|_| "slang activation thread panicked".to_string())?
```

## Defect 1: unregistered GC mutator

GC registration happens in exactly two places — `mutsu::gc_register_main_thread()`
(`src/lib.rs:47`, called once from `run_main`) and `spawn_registered_thread`
(`src/runtime/builtins_system.rs:65`). A raw `Builder::spawn` reaches neither, so
this thread is invisible to the collector while it builds an entire
`Interpreter`, loads a module, and runs that module's mainline — creating,
cloning, and dropping `Gc` values throughout.

`src/gc/stw.rs:34` states the rule: *"Only registered threads count toward
quiescence."* An unregistered thread's `Gc` mutation can land mid-scan and
corrupt Bacon-Rajan trial deletion. This is exactly the class of bug recorded in
the `gc-survivor-purple-verify-violation` writeup, whose conclusion was the
standing law that any thread touching `Gc` must be a registered spawn. The
`procasync-stress-segv` audit (`todo/deep/procasync-stress-segv.md` §4) walked
every `Proc::Async`-path thread to prove none of them violated it — and it did
not cover this path.

## Defect 2: bare `join()` outside `block_quiescent`

Every other blocking join in the runtime is wrapped so the waiting thread does
not starve stop-the-world: `native_proc_async.rs:712,715,724`,
`supply_transform.rs:141`, `worker_pool.rs:100`, and the convention is stated at
`builtins_system.rs:38`. The parent here is a registered mutator that blocks in a
bare `handle.join()`. If a collection is requested while it waits, STW waits for
the parent to reach a safepoint and the parent waits for the child — a deadlock
that only needs a collection to be triggered during a slang module load.

## Defect 3: default stack for code that can recurse arbitrarily

`main.rs:147` reserves 256 MiB for `mutsu-main` and
`builtins_system.rs:9`'s `USER_THREAD_STACK_SIZE` gives the same to every
`spawn_user_thread` worker, with the comment *"the default ~2 MiB thread stack
overflows on deep VM recursion"*. This thread gets the ~2 MiB default while
compiling and executing a module mainline — i.e. arbitrary user code, including
deeply-recursive grammar matching, which is the exact workload the 256 MiB
reservation exists for. A slang module with a non-trivial grammar would abort the
process with a stack overflow that no other module load can hit.

## Fix

Route it through the existing wrapper, which supplies the name, the stack size,
and the GC registration in one call, and wrap the join:

```rust
let handle = crate::runtime::builtins_system::spawn_user_thread(
    ACTIVATION_THREAD_NAME,
    move || -> Result<Vec<String>, String> { /* body unchanged */ },
);
crate::gc::block_quiescent(|| handle.join())
    .map_err(|_| "slang activation thread panicked".to_string())?
```

Two details to preserve:

- The thread **name** is load-bearing beyond diagnostics: `slang_activation.rs:31-34`
  documents that the parser hook reads it to refuse recursive activation.
  `spawn_thread` passes the name straight to `Builder::name`, so this survives —
  but a test should pin it, because the whole recursion guard silently disappears
  if it ever stops being set.
- `spawn_user_thread` bottoms out in `thread_compat::spawn_thread`, which does
  `.expect("failed to spawn worker thread")` on spawn failure, whereas the current
  code returns `Err("could not spawn slang activation thread: …")`. That turns a
  recoverable error into a panic. Either accept it (spawn failure here is
  unrecoverable in practice) or give `spawn_registered_thread` a
  `try_` variant; note the choice in the commit message.

## Why this is a ticket, not a deep item

No design decision is open — the correct policy already exists and is applied at
every other site; this one call predates or bypasses it. The change is under ten
lines. It is filed rather than fixed inline because it was found incidentally
while closing `news/2026-08/promise-spawn-segv-resolved-by-the-worker-pool.md`,
and it touches the slang path (ADR-0026), which deserves its own verification
pass rather than a drive-by edit.

## Verification

`t/slang-piersing-activation.t` and `t/slang-tuxic-activation.t` exercise the
path. Add coverage for the two invariants the fix restores:

- run both under the `gc-stress` configuration
  (`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`) and require zero
  `VERIFY FAIL`;
- assert the activation thread still reports `ACTIVATION_THREAD_NAME`, so the
  parser's recursive-activation guard keeps working.
