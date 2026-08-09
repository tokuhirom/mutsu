# Lexical named sub is unregistered when its declaring routine returns, breaking tap callbacks that fire later ("Unknown function")

## Affected tests

- `t/http2-frame-parser.rakutest` subtests 6, 8, 10, 12 — the flunked second leg ("Empty DATA frame", "DATA frame without padding", "DATA frame with zero padding", "DATA frame with padding"): the FrameSerializer never emits any Data frame, so `$complete` times out and `flunk $desc` fires at line 85.
- `t/http2-frame-serializer.rakutest` subtests 4, 5 ("Simple data frame is not parsed back!", "Simple data frame with padding is not parsed back!"): same mechanism — the serializer's `send-message($frame, True)` awaits a promise that is never kept.

All four failing frame-parser subtests and both frame-serializer subtests are Data frames because only the Data path passes `$consumes-window = True` to `send-message` (FrameSerializer.rakumod line 128-133), which emits a `WindowConsume` on `$connection-state.remote-window-change` and `await`s its promise. The promise is kept by `check-window-size` — a **lexical sub declared inside `submethod TWEAK` of `Cro::HTTP2::ConnectionState`** (ConnectionState.rakumod lines 25-61), called from the tap callback TWEAK registers. By the time the tap fires, TWEAK has long returned and mutsu has unregistered `check-window-size`, so the callback dies with `Unknown function: check-window-size`, the promise stays Planned, and the serializer hangs on `await`.

Confirmed in the real module via a shadowed ConnectionState with a `CATCH` probe in the tap:

```
SHADOW-CS: tap error: Unknown function: check-window-size
complete: Planned
```

## Repro

Minimal, pure Raku (`tmp/h2-wc-min2.raku` variant A):

```raku
class CA {
    has Supplier $.s = Supplier.new;
    method setup() {
        sub helperA($x) { say "A: helper $x"; True }
        $!s.Supply.tap: {
            CATCH { default { say "A CAUGHT: ", .message } }
            helperA($_);
        };
    }
}
my $a = CA.new;
$a.setup;
$a.s.emit(42);
sleep 0.3;
```

- mutsu (target/release/mutsu): `A CAUGHT: Unknown function: helperA`
- raku: `A: helper 42`

Boundary matrix (all verified, `tmp/h2-wc-min*.raku`):

| Scenario | mutsu |
|---|---|
| tap registered in method, emit while method frame still live | OK |
| tap registered in method/sub/TWEAK, emit after return | **FAIL** |
| closure stored in a var (assignment is the routine's last statement) and called after return | OK |
| tap registered inside a bare block (not routine) | OK |
| helper as `my &h = -> ... {}` instead of named sub | OK |

Cro-level repro (`tmp/h2-windowconsume.raku`, run via `bash tmp/croflake.sh` with `MUTSU_BIN` set to an absolute path): emits a `WindowConsume` into a fresh `Cro::HTTP2::ConnectionState` and prints `promise status: Planned` under mutsu, `Kept` under raku.

## Root cause

A routine body that declares inner routines snapshots the routine registry and restores it on return:

- `src/vm/vm_call_named.rs:27-48` (`call_compiled_function_named`) — and the same gate in `src/vm/vm_call_fast.rs:34/364` and `src/vm/vm_method_dispatch.rs:631/924` and `1533/1769`.
- The restore is skipped only when `return_value_escapes_routine(v)` (`src/vm/vm_call_light_typed.rs:654`) — i.e. when the **return slot** carries a Sub/Routine/Seq/LazyList.

This escape analysis misses every **side-channel escape**: a closure passed to `.tap` (our case), stored into an attribute, pushed onto an array, registered as a callback. After the restore, a later invocation of the escaped closure cannot resolve the inner sub because `find_compiled_function` (`src/vm/vm_call_resolve.rs:53-57`) requires `resolve_function_with_types` (the runtime registry) to succeed before it will use the compiled-fns table — the bytecode still exists in `compiled_fns` but is unreachable by name.

This is why the variant-A matrix looks the way it does: the "closure stored in var" cases work only because the assignment expression happens to be the routine's last statement, so the closure is also the return value and `return_value_escapes_routine` fires.

## Fix direction

Do not try to enumerate side channels statically (that is the incomplete-analysis trap CLAUDE.md warns about). Use a runtime over-approximation that cannot go flaky:

1. Add a monotonically increasing `closures_created: u64` counter on `Interpreter`, bumped in the closure-creation exec ops (`exec_make_block_closure` and siblings in `src/vm/vm_register_sub_ops.rs`; also the pointy/anon-sub creation ops if separate).
2. In each snapshot/restore gate (`vm_call_named.rs`, `vm_call_fast.rs`, `vm_method_dispatch.rs` x2), record the counter before running the body; skip `restore_routine_registry` when the counter changed during the call, in addition to the existing `return_value_escapes_routine` check.

Cost/risk: the snapshot path only runs when `cf.declares_inner_routines` is true (rare), so the over-approximation just means such a routine's inner subs occasionally outlive the call — a bounded registration leak, matching what the un-scoped method-dispatch path leaked unconditionally before the check existed (see the comment block at `vm_call_light_typed.rs:636-653`). A sharper variant (only skip when a created closure's compiled unit is nested inside this `cf`) can come later.

Note: the restore must still run for the loop-body case (`vm_for_loop_dispatch.rs`) — that one is about lexical scoping between sibling loops and is unrelated.

## Verification

- `tmp/h2-wc-min.raku`, `tmp/h2-wc-min2.raku` print no `CAUGHT:` lines and match raku output.
- `tmp/h2-windowconsume.raku` (via `croflake.sh`) prints `promise status: Kept`.
- `t/http2-frame-parser.rakutest`: 26/26 (currently 22/26).
- `t/http2-frame-serializer.rakutest`: subtests 4 and 5 pass (subtest 11 and the end-of-file abort are separate tickets: `http2-rw-param-buf-element-assign.md`, `http2-uint-enum-typecheck.md`).
- Existing suites: `make test`; watch `t/wrap.t`, `t/supply-*.t` — the registry restore interacts with sub redefinition tests.
