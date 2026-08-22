# `run`/`shell` silently discard the child's stdout/stderr instead of inheriting the parent's

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/ipc.rakudoc:14`).

## Root cause

In real Raku, `run`/`shell` (unlike a fully-captured `run(..., :out)`) let the child
process's stdout/stderr go straight to the same streams as the parent (OS-level
inheritance) when the caller does not ask to capture them. mutsu instead explicitly
redirects both to `/dev/null` whenever `:out`/`:err`/`:merge` are not requested:

```rust
// src/runtime/builtins_system_run.rs, builtin_run:
} else if opts.capture_out {
    cmd.stdout(std::process::Stdio::piped());
} else {
    cmd.stdout(std::process::Stdio::null());   // should inherit, not null
}
...
} else if opts.capture_err {
    cmd.stderr(std::process::Stdio::piped());
} else {
    cmd.stderr(std::process::Stdio::null());   // should inherit, not null
}
```

`builtin_shell` (same file, ~line 415-424) has the identical pattern. So any script that
calls `run`/`shell` without explicitly capturing output produces no visible output at all
in mutsu, where real Raku shows the child's output live (this is the normal/default way
scripts shell out and expect to see e.g. `git status`, `ls`, compiler/build tool output,
etc.).

## Minimal repro

```raku
run 'echo', 'hello';
```

- `raku`: prints `hello` (inherited stdout).
- `mutsu` (`target/debug/mutsu`): prints nothing.

Also affects `shell`:

```raku
shell 'echo hello';
```

- `raku`: prints `hello`.
- `mutsu`: prints nothing.

The doc's own example (`Language/ipc.rakudoc:14`, `run 'git', 'status';`) is exactly this
shape — the specific `git status` text itself is environment-dependent (repo state
varies), but mutsu producing **no output at all** regardless of repo state is the real,
reproducible bug underneath that finding.

## Why this is `todo/deep`, not a shallow one-line fix

- This is not obviously an accident — `Command::spawn()`'s default behavior already
  inherits stdio without any explicit `.stdout()`/`.stderr()` call, so someone had to
  deliberately write the `Stdio::null()` branches. It is worth checking whether this was
  done on purpose to keep child-process output from polluting TAP output during roast/test
  runs (`run`/`shell` appear inside several roast tests), in which case naively switching
  to inherited stdio could break existing whitelisted tests that currently rely on the
  child's output being suppressed by default.
- Fixing it requires auditing roast/`t/` coverage of `run`/`shell` for tests that
  implicitly depend on the current suppress-by-default behavior (a test whose PASS
  currently depends on a subprocess's stray output NOT reaching the TAP stream), not just
  flipping `Stdio::null()` to inherited and hoping nothing regresses.
- Affects two builtins (`run` and `shell`) with duplicated logic, and interacts with the
  existing `:merge`/`:out(handle)` special-casing already in the same functions, so the
  fix needs to preserve those paths' current (correct) behavior while changing only the
  true default (no capture options given at all) case.

## Affected files (starting point)

- `src/runtime/builtins_system_run.rs` (`builtin_run` around line 130-133,
  `builtin_shell` around line 415-424)
