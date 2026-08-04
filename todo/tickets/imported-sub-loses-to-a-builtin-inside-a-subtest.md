# A bareword call inside a `subtest` reaches the builtin instead of the imported sub

A `route` block with **more than one statement**, evaluated inside a `subtest`,
calls mutsu's builtin `get` (read a line from a handle) instead of
`Cro::HTTP::Router`'s exported `get`, and dies with `Expected IO::Handle`:

```raku
use Cro::HTTP::Router;
use Test;

subtest {
    my $a = route {
        get -> { content 'text/html', "x" }
        get -> 'y' { content 'text/html', "y" }
    };
    ok $a.defined, 'built';
}, 'probe';
```

`raku` builds the route. mutsu dies.

## The exact rule

`tmp/st6q.p6` runs three route blocks inside one subtest and gets:

| route body                          | result |
|-------------------------------------|--------|
| `get -> {…}` then `note "…"`        | **dies** (`Expected IO::Handle`) |
| `note "…"` then `get -> {…}`        | builds |
| `get -> {…}` then `get -> 'y' {…}`  | **dies** |

So the trigger is **`get` in a non-final statement position** of a route block
that runs inside a `subtest`. It is not `include` (any second statement will
do), not the number of statements as such, and not any particular route verb —
`note` first and `get` last is fine. These also all work:

- the same `route` block at file scope, in a bare block, in a sub, or in a
  `Callable` invoked by hand (`tmp/st6i.p6`);
- a `route` block inside a subtest with a **single** statement
  (`route { get -> { … } }`), where `get` is also the final statement;
- `route { note …; note …; get -> {…} }` inside a subtest (`tmp/st6p.p6`),
  which also shows that `&get` and `&content` *are* visible in the block's env
  (`(try &get).defined` is `True` there).

The final-statement/non-final split is the tell: a non-final statement is
compiled in sink context and reaches the **`ExecCall`** opcode, while the final
one goes through `CallFunc` — and only the `ExecCall` path misresolves.

## What the debugger shows

`rust-gdb` breaking on `src/runtime/handle_io.rs:119` (the `Expected IO::Handle`
site) gives the chain:

```
builtin_get                       (runtime/builtins_io_stream.rs:106)
call_function name="get"          (runtime/builtins.rs:1150)
exec_call_values name="get"       (runtime/call_helpers.rs:43)
exec_exec_call_op                 (vm/vm_call_exec_ops.rs:131)
… call_compiled_function_named fn_package="Cro::HTTP::Router" fn_name="route"
```

So the call compiled to the **`ExecCall`** opcode (the fallback for a name the
compiler could not resolve statically), and at run time
`find_compiled_function(compiled_fns, "get", args)` returned `None`, which drops
`exec_exec_call_op` into `exec_call_values` → `call_function` → the builtin. The
`compiled_fns` pointer is the same one the enclosing frames use, so the registry
is shared; what fails is `resolve_function_with_types("get", args)` inside
`find_compiled_function_inner` (`vm/vm_call_resolve.rs:53` — its `?` on the
fingerprint is the early return), i.e. the imported `&get` is not visible in the
env at that moment.

The subtest body is run through `call_sub_value` → `eval_block_value` →
`run_compiled_block` → `run_nested` (`runtime/test_functions/tap_subtest.rs:133`),
so the block is compiled at run time rather than with the file — which is the
likely reason the multi-statement form emits `ExecCall` where the single-statement
form does not.

## Not reproducible without Cro (yet)

Two synthetic attempts are **green** on mutsu, so do not chase a smaller repro by
guessing:

- `tmp/imp1.p6` + `tmp/implib/ImpTest.rakumod` — an exported `runner(&body)` and
  an exported `marker()` called by bareword in non-final position inside a
  subtest.
- `tmp/imp2.p6` + `tmp/implib/ImpTest2.rakumod` — the same with the exported
  multi named `get`, so it collides with the builtin.

Both are green, so the builtin-name collision and the non-final position are
*necessary but not sufficient*; something about how `route` invokes its block
(`Cro::HTTP::Router`'s `route` runs it under a `$*CRO-ROUTE-SET` dynamic scope,
and the subtest body itself is compiled at run time) is also in play. Grow
`tmp/st6q.p6` down instead of guessing at a smaller synthetic.

## Blast radius

`http-middleware.rakutest` subtest 6 ("Interaction of middleware written as
Cro::Transform with HTTP router"), which reports a silent `1..0`. Extracted from
the subtest into a bare block, all 11 of its assertions pass (`tmp/st6.p6`).
