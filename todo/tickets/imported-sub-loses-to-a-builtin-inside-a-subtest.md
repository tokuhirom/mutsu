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

`raku` builds the route. mutsu dies. All three of these work:

- the same `route` block at file scope, in a bare block, in a sub, or in a
  `Callable` invoked by hand (`tmp/st6i.p6`);
- a `route` block inside a subtest with a **single** statement
  (`route { get -> { … } }`);
- a `route` block inside a subtest whose first statement is not `get`
  (`route { include $inner; get -> { … } }` builds fine — `tmp/st6k.p6`).

So the trigger is "two or more statements, inside a subtest", not `include` and
not any particular route verb.

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
  an exported `marker()` called by bareword from a one- and a two-statement block
  inside a subtest.
- `tmp/imp2.p6` + `tmp/implib/ImpTest2.rakumod` — the same with the exported sub
  named `get`, so it collides with the builtin.

Grow `tmp/st6n.p6` (the minimal Cro repro above) down instead.

## Blast radius

`http-middleware.rakutest` subtest 6 ("Interaction of middleware written as
Cro::Transform with HTTP router"), which reports a silent `1..0`. Extracted from
the subtest into a bare block, all 11 of its assertions pass (`tmp/st6.p6`).
