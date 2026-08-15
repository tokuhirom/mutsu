# A `%*ENV` write becomes invisible to a spawned child's default environment once any OS thread has ever run

## Minimal repro

```raku
{
    my $p = Proc::Async.new: $*EXECUTABLE, '-e', 'say "hi"';
    my $stdout = '';
    $p.stdout.tap: { $stdout ~= $_ };
    my $prom = $p.start;
    await $prom;
}
temp %*ENV<PERL6_RUN_SHELL_ENV_TEST> = 'meows';
say run('sh', '-c', 'echo $PERL6_RUN_SHELL_ENV_TEST', :out).out.slurp(:close).trim;
```

Expected (and what raku prints): `meows`. mutsu prints an empty string — the
spawned `sh` does not see the env var `temp` just set, even though mutsu's own
`%*ENV<PERL6_RUN_SHELL_ENV_TEST>` reads back as `meows` immediately after the
assignment.

**The trigger is not Proc::Async-specific.** The bare block above can be
replaced with anything that spawns an OS thread via
`Interpreter::clone_for_thread()` + `crate::runtime::worker_pool::submit*` —
confirmed with nothing but:

```raku
{
    my $done = Promise.new;
    my $count = 0;
    Supply.interval(0.05).tap({ $count++; $done.keep if $count >= 2 });
    await Promise.anyof($done, Promise.in(2));
}
temp %*ENV<PERL6_RUN_SHELL_ENV_TEST> = 'meows';
say run('sh', '-c', 'echo $PERL6_RUN_SHELL_ENV_TEST', :out).out.slurp(:close).trim;
```

Same empty result. **Not a timing race**: adding `sleep 1` between the block
and the `temp`/`run` does not fix it — the corruption is persistent for the
rest of the process, not a window that closes once the thread finishes.

## What is confirmed, and what is not

- `%*ENV<key> = val` genuinely updates mutsu's own in-memory `%*ENV` hash
  (`self.env.get("%*ENV")` reads back the new value immediately) — this part
  is correct.
- `%*ENV<key> = val` is *also* wired to call `std::env::set_var` (see the
  `SAFETY:` comments in `vm_var_assign_element.rs` /
  `vm_var_assign_index_named.rs`), presumably so a **subsequently spawned
  child process that relies on `Command::spawn()`'s default OS-level env
  inheritance** (no explicit `:ENV`/`:env`) picks it up automatically.
- Passing the value **explicitly** (`run(..., :env(%*ENV))`) works correctly
  every time, proving mutsu's own `%*ENV` state is right and the general
  "build a child's env from a given hash" path (`cmd.env_clear()` +
  `cmd.env(k, v)` in a loop) is sound.
- Only the **default-inheritance** path is affected, and only after at least
  one OS thread has been spawned via the `clone_for_thread` +
  `worker_pool::submit`/`submit_joinable` mechanism (used by every
  channel-backed live tap: signals, sockets, `Supply.interval`, and — after
  `todo/tickets/procasync-stdout-is-not-incremental.md` landed —
  `Proc::Async` taps registered before `.start()`).
- Not investigated: whether `std::env::set_var`'s call genuinely fails to
  reach the OS-level `environ` once another thread exists (a documented class
  of hazard — `std::env::set_var` is `unsafe` in recent Rust specifically
  because of exactly this: concurrent access from another thread is UB on
  some platforms), or whether `Command::spawn()`'s own env-reading path
  becomes stale/cached once other threads exist, or whether this is a
  `worker_pool`-specific interaction (its threads' own setup/signal-mask
  handling, thread-pool reuse, etc.) rather than a generic
  "any second thread" hazard. No experiment isolated `worker_pool` from
  "any second OS thread at all" (e.g. a bare `std::thread::spawn` with no
  mutsu machinery involved was not tried).

## Why it is deep, not a quick patch

This sits at the intersection of Rust's documented `std::env::set_var`
thread-safety hazard and mutsu's own threading infrastructure
(`clone_for_thread`, `worker_pool`, the GC's registered-mutator threads via
`spawn_gc_helper_thread`). A real fix needs to either:

1. **Stop relying on `std::env::set_var`/OS-level inheritance at all** for
   `%*ENV` and always build every spawned child's environment explicitly from
   mutsu's own `%*ENV` hash — the workaround applied to `Proc::Async.start()`
   in `todo/tickets/procasync-stdout-is-not-incremental.md`'s PR. This is
   probably the right direction generally (it sidesteps the hazard by
   construction and matches what `%*ENV` is supposed to mean authoritatively),
   but needs an audit of every other spawn site that currently relies on
   default inheritance (`shell()`, any other `Command::new(...).spawn()` call
   without explicit envs) to apply the same fix, plus a decision on whether
   `run`/`shell` (which are presumably implemented in terms of `Proc::Async`
   already) automatically inherit the fix or need their own.
2. Or root-cause **why** `std::env::set_var` stops reaching a later
   `Command::spawn()` once a thread exists in this specific codebase — trace
   it with `rust-gdb` breakpoints on `std::env::set_var`'s libc `setenv` call
   and on `Command::spawn()`'s env-reading path, confirmed against a MINIMAL
   Rust program outside mutsu entirely (no `worker_pool`, no GC, just
   `std::thread::spawn(|| {}).join(); std::env::set_var(...);
   Command::new("sh")...`) to establish whether this is a genuine
   upstream Rust/libc/OS hazard mutsu cannot avoid, or something mutsu's own
   threading setup (signal handling, GC mutator registration, thread-local
   state) specifically triggers.

## Affected files

- `src/vm/vm_var_assign_element.rs`, `src/vm/vm_var_assign_index_named.rs`,
  `src/vm/vm_var_delete_ops.rs` — the `std::env::set_var`/`remove_var` calls
  for `%*ENV` element assignment/deletion.
- `src/runtime/runtime_thread.rs` — `clone_for_thread`/`clone_for_thread_excluding`.
- `src/runtime/worker_pool.rs` — `submit`/`submit_joinable`, the pooled worker
  threads every live tap runs its callback on.
- `src/runtime/native_proc_async.rs` — worked around locally for the default
  (no `:ENV`) case by explicitly applying `self.env.get("%*ENV")` instead of
  relying on inheritance.
- Any other `std::process::Command::spawn()` call site that relies on default
  env inheritance without this workaround remains exposed (not audited).
