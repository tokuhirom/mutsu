# A panic caught mid-call still leaks manually-saved call-context state (`current_package`, pragma state, ...)

Found while triaging `t/vm-panic-boundary.t`
(`todo/tickets/local-tests-rely-on-a-lenient-native-is.md`), which fails under
`MUTSU_REAL_TEST=1` (the vendored real `Test.rakumod`) but passes fully (9/9)
under mutsu's native `Test` provider — the two providers invoke a tested block
through different call paths, and only one of them exposed this.

## What is fixed (this session, `src/vm/vm_env_helpers.rs`,
`src/vm/vm_run_loop.rs`)

A Rust panic (integer overflow, index OOB, capacity overflow, ...) raised
inside a nested closure/sub call is caught at the nearest `catch_unwind`
boundary (`run_inner_guarded` for `EVAL`/top-level, `run_range_guarded` for
`try`/`CATCH`) and converted to a catchable `X::AdHoc`. But every call frame
pushed between that boundary and the panic site is normally popped by its own
return-path cleanup (`pop_call_frame`, called near the end of
`call_compiled_closure_with_topic` / `call_compiled_function_named_inner`) —
code that a Rust unwind skips entirely (only `Drop` runs on unwind; a plain
statement near a function's end does not). So `self.call_frames` was left
holding every un-popped frame, and `self.locals`/`self.upvalues`/`self.env`
still belonged to the deepest panicking callee instead of the code resuming at
the boundary. The very next `GetLocal` in the resuming code used a slot number
valid for **its own** locals array, not the leftover (usually much smaller)
callee one — an immediate secondary index-out-of-bounds panic that aborted the
whole process, even though the *original* panic was supposed to be safely
caught (reproduced standalone, no Test module involved: `try { (-> { my @a;
@a[2**64-1] = 1 })() }; say $some_earlier_local;`).

Fixed generally: `Interpreter::recover_call_frames_after_panic(entry_depth,
entry_stack_depth)` pops every `call_frames` entry pushed since the boundary
was entered (each `pop_call_frame` naturally restores its caller's
`locals`/`upvalues`/`env`/etc., since frames are LIFO) and truncates the value
stack back to the boundary's entry depth. Wired into both `catch_unwind` sites.
Regression test: `t/panic-recovery-call-frames.t`.

## What is still open

`current_package` (and likely other similarly-shaped state) is saved and
restored **outside** the `call_frames` mechanism, as a plain Rust local in the
dispatch function itself:

- `call_compiled_closure_with_topic` (`src/vm/vm_closure_dispatch.rs`, around
  line 729): `let saved_pkg = { ... self.set_current_package(pkg) ... }`,
  restored near the function's end — skipped on unwind, same as the
  `pop_call_frame` gap above.
- `call_compiled_function_named_inner` (`src/vm/vm_call_named_inner.rs`, line
  153): `let saved_package = self.current_package().to_string(); ...
  self.set_current_package(def_package)`, restored at line 562 — same gap.
- `call_compiled_closure` also saves/restores pragma state
  (`save_pragma_state`/`restore_pragma_state`, `vm_closure_dispatch.rs` around
  lines 115/140) the same unguarded way.

Repro (no Test module needed — mirrors the real `Test.rakumod`'s `dies-ok`
shape, a named sub whose body calls a Callable via `$code()` inside its own
`try`, then calls a sibling function in its own package after the `try`):

```raku
# tmp/panic-named-sub-repro2.raku-style, but with the sibling function
# declared in a *package* (not lexically `my sub`) to exercise
# current_package-based unqualified resolution, matching Test.rakumod's
# `unit module Test;`-scoped `proclaim`.
```
`MUTSU_REAL_TEST=1 target/debug/mutsu t/vm-panic-boundary.t` still shows this:
after the recovery fix above, execution correctly continues past the panic
(no more crash / no more locals corruption), but the very next unqualified
call inside `dies-ok` (`proclaim(...)`, a `unit module Test;`-scoped sub)
fails with `Unknown function: proclaim` — `self.current_package` was left as
whatever the *panicking closure's own* package was (typically `GLOBAL`, from
the user's `-> { ... }` block), not restored to `Test`, because
`call_compiled_closure_with_topic`'s `saved_pkg` restore was skipped on
unwind exactly like `pop_call_frame` was.

## Why this is a separate, larger task

Fixing it fully means auditing every call/dispatch function for
manually-saved-and-restored state that lives outside `VmCallFrame` (at least
`current_package`, pragma state; there may be more — `env` overlay handling
also has adjacent manual bookkeeping) and either:

1. Moving each one into `VmCallFrame` itself (so
   `recover_call_frames_after_panic`'s existing pop-loop restores it for
   free) — requires restructuring the save point to happen at/before
   `push_call_frame()` in each function, which is not always where the value
   is naturally computed today (e.g. `current_package` is set well after
   `push_call_frame()` in `call_compiled_closure_with_topic`), or
2. Converting each site to an RAII guard (a struct whose `Drop` restores the
   field, constructed right after the mutation) so it self-heals on unwind
   regardless of what a future boundary looks like.

Either route touches multiple hot call-dispatch functions
(`vm_closure_dispatch.rs`, `vm_call_named_inner.rs`, possibly
`vm_call_named.rs`/`vm_call_dispatch.rs`) and needs care not to regress the
*normal* (non-panicking) return path's existing merge/writeback semantics,
which is more invasive than the `call_frames` fix above (that one only needed
a new pop-loop; this one needs new fields threaded through several already-
large functions, or a guard-type migration). Worth doing as its own
architectural slice, not a quick follow-up.

## Current test status

- `t/vm-panic-boundary.t`: 9/9 under mutsu's native `Test` provider (what
  `make test`/CI actually runs) — unaffected either way, since the native
  provider invokes the tested block through a path that already has a close
  catch_unwind boundary and never accumulates enough un-popped side-channel
  state to hit this. Still 6/9 under `MUTSU_REAL_TEST=1` (up from an earlier,
  harder failure mode before this session's `call_frames` fix — previously it
  crashed via locals corruption; now it fails cleanly on the `current_package`
  gap above, one subtest later).
- `t/panic-recovery-call-frames.t` (new): pins the `call_frames`/locals/stack
  recovery fix that IS shipped, independent of the `current_package` gap.
