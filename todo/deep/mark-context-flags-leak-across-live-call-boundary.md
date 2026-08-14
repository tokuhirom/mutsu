# VM "mark context" flags (`bind_context` et al.) leak across a live function-call boundary

## Symptom

`Crypt::RC4`'s own test suite (`t/00basic.t`, one of the un-triaged `test_die`
rows in [todo/tickets/dist-test-suite-failures-batch.md](../tickets/dist-test-suite-failures-batch.md))
dies with `Cannot modify an immutable Range` while running code that looks
completely unrelated to the actual bind expression:

```raku
class Crypt::RC4 {
    has uint8 @!state;
    submethod TWEAK(Blob() :$key!) {
        @!state := setup( $key );   # <-- the bind
    }
}
sub setup( $key --> array[uint8] ) {
    my uint8 @state = 0..255;       # <-- fails HERE: @state stays a Range
    ...
    @state[$x] = ...;               # "Cannot modify an immutable Range"
    @state;
}
```

Minimal repro (no module, no Blob coercion, no RC4 specifics):

```raku
class Foo {
    has uint8 @!other;
    method go() {
        @!other := make();   # a normal function CALL, not an inline do-block
    }
}
sub make() {
    my uint8 @state = 0..5;
    @state[2] = 99;   # dies: Cannot modify an immutable Range (0 1 2 3 4 5)
    @state;
}
Foo.new.go;
```

Raku: no error, `$obj.other` is `[0 1 99 3 4 5]`.

## Root cause

`MarkBindContext` is a VM opcode that sets `self.bind_context = true`
(`vm_exec_dispatch.rs` `OpCode::MarkBindContext` arm). It is emitted by the
compiler right before a `:=` bind target's own store op, meant to be consumed
by the very next `SetLocal`/`SetGlobal`-family opcode
(`vm_var_assign_set_local.rs:256,352`) so that op knows to preserve the RHS
container instead of copying it.

`self.bind_context` is a single `Interpreter`-wide field, not scoped to a
call frame. `Foo::go`'s compiled body is:

```
0: MarkBindContext
1: MarkRebindContext
2: LoadConst(1)
3: CallFuncNamed { name_idx: "make", arity: 1, ... }
4: ContainerizePair
5: SetGlobal(0)          # @!other's real store — the intended consumer
6: GetGlobal(0)
```

Between `MarkBindContext` (instr 0) and its intended consumer `SetGlobal`
(instr 5) sits a full function CALL (instr 3). `make()`'s own body executes
with `self.bind_context` still `true` (nothing clears it for a normal call),
so `make`'s own `my uint8 @state = 0..255;` declaration is wrongly compiled^Wexecuted
as if IT were a bind target too, skipping the Range-to-array materialization
a typed native array needs — hence the later index assignment finds a bare
Range in the slot.

This is a **runtime** analog of a **compile-time** bug fixed alongside this
finding in `src/compiler/stmt.rs` (the `bind_vardecl` flag leaking into a
nested `my`-declared variable inside a `do {}` block bound via `:=` — see
`t/bind-do-block-nested-vardecl-leak.t`). That fix only covers same-compile-unit
inlining (`do {}` blocks, `compile_block_inline`); it cannot help here because
`make()` is a genuinely separate compiled function invoked through a real
`CallFuncNamed` — there is no static/AST-level relationship for the compiler
to see, and the leak happens at VM runtime instead.

## Why this needs a design pass, not a quick patch

`self.bind_context` is one of a *family* of similar one-shot VM context flags
that share the exact same shape (set before a Set-op, meant to be consumed
immediately after): `scalar_bind_context`, `rebind_context`,
`constant_context`, `array_share_context`, `vardecl_context`,
`explicit_initializer_context`, `param_raw_bind_context`. Any of them could in
principle leak the same way across a call.

There is already a **precedent fix** for exactly this class of bug:
`vm_run_loop.rs`'s "nested run" boundary (used for `EVAL`, `dies-ok`/`lives-ok`
blocks, etc.) explicitly saves, clears, and restores this entire flag family
around `f(self)` (lines ~324-410). That mechanism does NOT run for an ordinary
compiled function/method call — those go through a flat bytecode dispatch
loop with call frames pushed in-place, not a nested Rust-level `run()`
invocation, so there is no single boundary to hook.

A correct general fix needs to:

1. Identify **every** call-boundary function that dispatches into a callee's
   compiled body without going through `vm_run_loop.rs`'s nested-run save/
   restore — at least `call_compiled_function_light_spec`
   (`vm_call_light_typed.rs`), `vm_call_light.rs`, `vm_call_fast.rs`'s
   positional-light path, `call_compiled_function_named`, and
   `call_compiled_closure` (`vm_closure_dispatch.rs`) for method calls. These
   already save/restore an ad-hoc set of caller-local state (see
   `call_compiled_function_light_spec`'s `saved_loop_local_vars` /
   `saved_block_declared_vars` / `saved_active_loop_param_names` pattern) —
   the "mark context" flag family should join that existing isolation
   pattern, not invent a new mechanism.
2. Decide whether to isolate the whole flag family (matching
   `vm_run_loop.rs`'s existing precedent) or just `bind_context` (the one
   flag proven to leak so far) — the former is safer against the next
   instance of this bug class but touches every call boundary; audit for
   perf impact on the hot light-call path (this is exactly the path recent
   sessions have been tuning — see `docs/adr/0001-gc-strategy-and-phasing.md`
   §7 JIT/light-call work).
3. Verify no call boundary *relies* on the current leaky behavior (e.g. a
   tail call intentionally propagating one of these flags) before clearing
   unconditionally — grep each flag's write sites first.

## Affected / blocked

- `Crypt::RC4`'s own test suite (`t/00basic.t` in
  [todo/tickets/dist-test-suite-failures-batch.md](../tickets/dist-test-suite-failures-batch.md)'s
  un-triaged list) — blocked on this.
- Any other dist/user code with the shape `@!attr := some_sub_call()` where
  `some_sub_call` internally declares and mutates a typed native array (or
  any other construct whose materialization depends on one of these
  bind-family flags being unset).

## Repro files

Kept as throwaway scratch, not committed:
`tmp/t10.raku` (minimal, no dist), `tmp/rc4-inline.raku` (full Crypt::RC4
flow inlined, no module boundary) — regenerate from the "Minimal repro" block
above if needed; `tmp/` is gitignored so nothing to clean up.
