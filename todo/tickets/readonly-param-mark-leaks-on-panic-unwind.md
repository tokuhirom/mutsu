# A parameter's readonly mark leaks past its call on a panic unwind

Found while building a regression test for
`todo/tickets/thread-param-mask-leaks-on-panic-unwind.md` (now fixed via
`ThreadParamMaskGuard`, see `news/2026-08/thread-param-mask-panic-unwind-raii-guard.md`).
This is a **separate mechanism** — the readonly marking a routine parameter
gets by default, not `thread_redeclared_vars`/`thread_param_shadow_vars` —
that appears to suffer the exact same "manually restored, skipped by a Rust
panic unwind" bug shape.

## Repro (confirmed, no threading involved at all)

```raku
sub victim($desc) {
    my @a; @a[2**64 - 1] = 1;   # deliberate Rust panic (index-OOB add overflow)
}

my $desc = 'outer-initial';
try { victim(999) };
$desc = 'outer-updated';        # !!! fails
say $desc;
```

Running this with `target/debug/mutsu` produces the underlying Rust panic
(caught by the `try`/`run_range_guarded` boundary, as expected — see
`t/vm-panic-boundary.t`), but the NEXT statement then fails with:

```
Cannot assign to a readonly variable (desc) or a value
  in sub victim at ... line 6
  in block <unit> at ... line 6
```

i.e. the outer `$desc` — a completely unrelated lexical that merely shares a
bare name with `victim`'s own parameter — is left permanently readonly after
`victim`'s panicking call, even though `victim` itself never returned
normally. No `start{}`/`shared_vars_active` is involved at all; this
reproduces in a single-threaded program.

## Likely root cause (unconfirmed — needs a session to trace precisely)

`call_compiled_function_named_inner` (`src/vm/vm_call_named_inner.rs`) already
has one related comment: *"A routine gets a fresh, writable `$_` — clear any
readonly mark leaked from the caller's topic ... param binding below re-marks
`_` for an explicit `$_` parameter"* — implying the general
`mark_readonly`/`unmark_readonly` machinery is already known to be a source of
leaks in at least one other case (the topic). A routine parameter is
presumably marked readonly on entry (Raku's default: a sub parameter is `is
readonly` unless declared `is rw`/given a sigilless bind) and unmarked when
the call returns, likely via another plain `self.mark_readonly(...)` /
`self.unmark_readonly(...)` statement pair rather than an RAII guard — the
same shape `ThreadParamMaskGuard`, `StateScopeGuard`, `WhenMatchedGuard`,
`CurrentPackageGuard`, `MarkContextGuard`, and `PragmaGuard` were all added to
fix for other pieces of call-dispatch state (see
`news/2026-08/panic-unwind-call-dispatch-state-raii-guards.md` and the
thread-param-mask fix above). A Rust panic raised inside `victim`'s body
(between the mark and the manual unmark) would unwind straight past the
unmark statement exactly like the other fixed cases, leaving "desc" marked
readonly in whatever registry/set backs `mark_readonly`/`unmark_readonly`
(need to find its actual storage — grep `fn mark_readonly`/`fn
unmark_readonly`/`fn no_readonly_vars` in `src/`) after the panic is caught.

## Why this is filed separately rather than folded into the thread-param-mask fix

- Different backing state (a readonly-name registry, not
  `thread_redeclared_vars`/`thread_param_shadow_vars`).
- Reproduces with **zero threading** — this is a plain single-threaded
  correctness bug, unrelated to the cross-thread shared-var masking the
  sibling ticket's fix addressed. Bundling it in would have conflated two
  distinct root causes in one PR.
- Not yet root-caused precisely (which function pair does the mark/unmark,
  where exactly the manual restore statement lives) — that investigation
  should happen in its own session, likely following the exact same
  RAII-guard recipe (`vm_call_state_guard.rs`'s `StateScopeGuard`/
  `WhenMatchedGuard` v3 pattern, or `CurrentPackageGuard`'s `Arc`-based
  pattern if the readonly registry already has interior-mutable backing).

## Suggested next step

1. Grep `fn mark_readonly`, `fn unmark_readonly`, `fn no_readonly_vars` to find
   the backing storage type and every call site.
2. Confirm whether the mark/unmark pair for an ordinary routine parameter is a
   plain statement pair (matching this bug shape) or already RAII-guarded
   somewhere.
3. If a plain pair, apply the same guard recipe used for the sibling fixes:
   either wrap the backing field in `Box<Cell<T>>`/`Box<RefCell<T>>` for a raw-
   pointer v3-pattern guard (if it's a plain field), or use the `Arc`-based
   `CurrentPackageGuard` pattern (if it already has interior mutability).
4. Add a regression test starting from the minimal repro above.
