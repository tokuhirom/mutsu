# `http-router-named-urls.t` (Cro::HTTP) times out (rc=124) after its own assertions pass

## Context

Not in `roast/` — this is Cro::HTTP::Router's own test suite, checked out
under `tmp/cro-work/`. Referenced as a known, separate issue in the (now
resolved) `closure-for-loop-param-hijacked-by-same-named-captured-outer`
ticket, which fixed a `for`-loop param `GetUpvalue` hijack that made this
file's "Escaped named param" / "Escaped positional" subtests fail. Those
two subtests now pass (verified 2026-08-11) — but the file as a whole still
times out:

```
bash -c 'INC=$(cat tmp/cro-work/inc-paths.txt); \
  timeout 120 target/debug/mutsu $INC -I tmp/cro-work/C_RO_CRO_HTTP_.../t \
    tmp/cro-work/C_RO_CRO_HTTP_.../t/http-router-named-urls.t'
# rc=124, ok=28 (all individually-passing subtests up to that point), no "not ok"
```

## Not yet diagnosed

No repro isolation, no gdb backtrace of where execution is stuck, and no
check of whether this reproduces on a release build (a debug-build timeout
alone is not conclusive — see CLAUDE.md's flaky-triage guidance: confirm
with `target/release/mutsu` before assuming a real hang, since a heavy
debug-build test can simply be slow rather than stuck).

## Suggested attack

1. Confirm with a release build first (rules out "just slow in debug").
2. If still timing out at release speed, `rust-gdb -batch -ex run -ex bt`
   attached to the hung process (or a manual `timeout 30 ... &` + `gdb -p`
   attach) to find the stuck opcode/call.
3. Binary-search the test file's subtests (comment out the back half) to
   narrow which specific assertion or fixture setup after subtest 28 hangs.

## 2026-08-11 investigation (narrowed further, still unresolved)

Confirmed with both debug and release builds, 3+/3 reproductions each, via
`bash tmp/cro-t.sh t/http-router-named-urls.t`: the hang is fully
deterministic (not load-sensitive) and always stops at the exact same
point — TAP shows `ok 30 -` (no description) as the last line, then hangs
until killed.

**Exact hang site identified.** `ok 30` is the *first* `is` assertion inside
this block (source lines 104-110):

```raku
test-route-urls route {
    get -> {
        is abs-link('css'), '/css';                           # <- this one: ok 30
        is abs-link('css', 'x', 'y', 'z'), '/css/x/y/z';       # <- hangs evaluating this one
    }

    get :name<css>, -> 'css', +a { };
}
```

Both assertions run inside the *same* handler invocation for the *same*
synthetic `GET /` request built by the `test-route-urls` helper (see top of
file): `Supplier.new` → `$app.transformer($source.Supply).Channel` →
`$source.emit(...)` → `$responses.receive`. So the hang is either (a) inside
`abs-link('css', 'x', 'y', 'z')` itself — generating a link for a route
whose only handler-side param is a bare (no-sigil) slurpy positional
`+a` — filling it with 3 extra args, or (b) in whatever machinery is
supposed to deliver the handler's completed response back through the
Channel afterward.

**Confirmed genuine deadlock, not an infinite/spinning loop.** `ps
-o pid,etimes,time,pcpu,nlwp` on the hung process shows CPU time flatlines
early (~6s of CPU time accumulated by ~6s wall-clock, then *zero* additional
CPU time consumed over the next several seconds while still hung) — i.e.
by the time it's stuck, no thread is doing computational work. All 4
threads (`/proc/<pid>/task/*/status`) sit in state `S` with `wchan
futex_do_wait`. A `perf record -e sched:sched_switch --call-graph dwarf`
sample of the periodically-timed-out thread shows it repeatedly parked in:

```
mutsu::runtime::methods_call_dispatch::call_method_with_values
  -> mutsu::runtime::methods_promise::dispatch_channel_method
  -> mutsu::value::value_async::<impl SharedChannel>::receive_result
  -> mutsu::gc::stw::wait_until -> stw_aware_wait -> Condvar::wait_timeout
```

i.e. it is literally sitting in the `.receive()` call of `test-route-urls`,
waiting for a value that never arrives — meaning nothing in the request
pipeline ever called `SharedChannel::send`/`close`/`fail` on this route's
response Supply. The other 3 threads never woke during the sampling
window (no timeout on their wait), so their stacks weren't captured —
`ptrace` is not available in this environment (`rust-gdb -p` fails with
`Could not attach to process` / yama `ptrace_scope`; only `perf` has
passwordless sudo here, not `gdb`), so a full 4-thread backtrace at the
moment of hang is still missing. Getting one (e.g. by asking the user to
temporarily lower `ptrace_scope`, or starting `perf record
-e sched:sched_switch --call-graph dwarf -a` *before* launching the
process so switch-out events for all 4 threads land inside the recording
window) is the natural next step.

**Trap hit while trying to build a synthetic repro — do not lose more time
to this:** editing/truncating `t/http-router-named-urls.t` in *any* way
(even adding a handful of `note` calls, or truncating well past the
target block) reliably flips the failure mode to `Runtime error: Can't use
unknown trait 'is' -> 'cookie' in a parameter declaration` — a **different,
already-tracked, size-sensitive parse-time bug**
(`todo/deep/pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules.md`).
Verified this is NOT a shared-cache race (`MUTSU_PRECOMP=0` doesn't change
it) — it really is in-process ParseMemo pointer-identity sensitivity to the
compiled buffer's exact byte length. **Do not try to instrument this
specific file by editing it** — any edit changes its length and falls into
that unrelated bug instead. A Cro-independent minimal repro (own file,
outside the `t/`/`-I` tree) has not been achieved yet either — two attempts
both resolved `abs-link` calls fine outside of Cro's own module tree,
because `abs-link`/`route{}` need context normally only set up by
`Cro::HTTP::Router` internals (not yet replicated standalone).

**Suggested next steps**, in order:
1. Get a full 4-thread native backtrace at the hang (ptrace access needed —
   ask the user, or find another route to it) rather than editing the `.t`
   file.
2. Alternatively, add temporary `eprintln!`-based instrumentation on the
   **Rust side only** (e.g. in `native_supply_dispatch.rs`'s `"Channel"`
   arm, and wherever a spawned thread executes a `whenever`/handler body
   and would normally call `ch.send`/`ch.fail`) gated behind a scratch env
   var — this avoids touching the `.raku` file's byte length entirely and
   sidesteps the ParseMemo trap above.
3. Read `Cro::HTTP::Router`'s route-matching/link-generation source
   (`tmp/cro-work/C_RO_CRO_HTTP_*/lib/Cro/HTTP/Router.rakumod`) for how it
   handles a slurpy positional (`+a`) with no sigil when building/matching
   a link — compare against `raku`'s behavior for the same construct
   (`raku` passes this test file fully) to see whether mutsu's slurpy
   handling for a **bare (no-sigil) slurpy** specifically is diverging.
