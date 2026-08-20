# `use fatal` does not immediately explode a Failure nested in function-call argument marshalling

**Residual scope after the list/array/hash-literal fix** (see
`news/2026-08/fatal-mode-composite-literal-failure.md`): composite-*literal*
construction (`(...)`, `[...]`, `%(...)`) now explodes correctly under `use
fatal` when one of its element expressions produces an unhandled `Failure`
— `Interpreter::explode_if_fatal_failure_in_composite()` is called from
`exec_make_array_op`/`exec_make_array_no_flatten_op`/`exec_make_hash_op`/
`exec_make_hash_from_pairs_op` in `src/vm/vm_data_ops.rs`, right after those
opcodes collect their element values off the stack and before the composite
becomes a stored value.

What that fix does **not** cover: a `Failure` produced by one *argument
expression* of a plain function/method call. Real `raku` explodes there too:

```raku
use fatal;
sub f($a, $b, $c) { say "in f" }
f(1, "a".Int, 3);
say "reached";
```

`raku`: throws `Cannot convert string to number: ...` at the `f(...)` call
(never reaches `say`, never even runs `f`'s body). `mutsu` (current): calls
`f` with the Failure as `$b`, prints `in f`, then `reached`.

## Why this needs a separate fix

Unlike list/array/hash literals, a plain call's arguments are **not**
assembled through `MakeArray`/`MakeHash` first — `CallFunc`/`CallFuncNamed`/
`CallMethod`/`CallMethodDynamic`/etc. (see `src/vm/vm_call_ops.rs` and
`src/vm/vm_call_exec_ops.rs`) pop their `arity` argument values directly off
the VM stack inside their own `exec_call_*_op` helpers — there is no single
composite-construction chokepoint to hook the way there was for
`MakeArray`/`MakeHash`. Reaching parity for calls means adding the same
`explode_if_fatal_failure_in_composite`-style scan (reusing that helper,
already in `src/runtime/accessors.rs`) at each of those argument-popping
sites — a wider set of call sites than the composite-literal fix touched,
and one that needs care not to double-explode a Failure a callee's own
`fatal_mode` handling (e.g. a `try` around the call) already deals with.

## Minimal repro

```raku
use fatal;
sub f($a, $b, $c) { say "in f" }
f(1, "a".Int, 3);
say "reached";
```

`raku`: throws before `f` ever runs. `mutsu`: prints `in f` then `reached`.
