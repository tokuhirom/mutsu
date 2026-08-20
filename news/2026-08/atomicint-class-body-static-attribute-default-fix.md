# Fix `my atomicint` class-body statics read via `⚛++` from an attribute default

A class-body `my atomicint $x` read with `⚛++`/`⚛--` inside an attribute's
default-value expression was wrong for the first constructed instance and
off-by-one for every instance after that:

```raku
class Foo {
    my atomicint $current-id = 1;
    has $.id = $current-id⚛++;
}
my $a = Foo.new;
my $b = Foo.new;
say $a.id;  # was: (atomicint)  -- now: 1
say $b.id;  # was: 1            -- now: 2
```

## Root cause

An attribute default-value expression compiles as its own standalone bytecode
chunk with an EMPTY local-slot table (`Compiler::new_decl_chunk_compiler`),
because it must resolve free variable names through whatever environment the
declaration registers in, evaluated fresh at each instance's construction. A
plain (non-atomic) class-body `my Int $x` used the same way works correctly
because the normal `PostIncrement` opcode falls back through several
env-and-beyond resolution paths, including `package_scope_lexical` /
`read_package_scope_var` -- the per-package "static" store
(`Interpreter::package_lexicals`) that is where a class-body `my` lexical
actually lives.

The `⚛++`/`⚛--` postfix operators, however, compile to a runtime call to a
builtin (`__mutsu_atomic_post_inc_var`) by the variable's bare NAME as a
string literal, bypassing all compiler-level variable resolution. That
builtin's variable-lookup helpers (`atomic_scalar_cell`,
`canonical_atomic_var_name`, `atomic_current_value`) only ever consulted
`self.env` (plus a process-global name-keyed "legacy" atomic lane) -- never
`package_lexicals`. So the first read found nothing in `env`, fell back to a
type-constraint-derived placeholder (`(atomicint)`, the visible symptom), and
every instance after that read/wrote the process-global legacy lane instead
of the real class-body static, producing a value shifted by one instance.

Confirmed with `rust-gdb` breakpoints on `atomic_current_value` (in
`src/runtime/builtins_atomic.rs`): `self.env.contains_key("current-id")` was
`false` at the very first atomic call, while `self.package_scope_lexical
("current-id")` and `self.read_package_scope_var("current-id")` both
returned `Some(1)` (the correctly-initialized class-body static) at that
exact same point.

## Fix

`Interpreter::atomic_scalar_cell` (`src/runtime/builtins_atomic_shared.rs`)
now falls back to a new `box_package_scope_lexical_cell` helper when a name
has no frame-local slot to box: it looks the bare name up in
`self.package_lexicals[current_package]`, boxes the value into a shared
`ContainerRef` cell (mirroring the existing frame-local-slot boxing logic
just above it), and writes the cell back into `package_lexicals` so every
subsequent atomic op -- from any frame, for any instance -- reads and writes
the exact same cell. This routes the class-body static through the same
mutex-protected read-modify-write primitive every other atomic scalar uses,
rather than adding a new name-keyed lookup path.

Verified against `raku` as the oracle for: the ticket's exact repro (2
instances), 4 instances (confirming the fix is not merely a shifted
off-by-one), and a native `has int $.id` variant (the ticket noted the bug
reproduced identically for native and non-native attributes). All match.

As a bonus check, `modules/Log-Timeline`'s `t/logging.rakutest` (the
motivating real-world case, `Log::Timeline::Ongoing::Logged`'s task-ID
counter) now progresses past the tests that were previously aborting with
`X::TypeCheck::Binding::Parameter` from the wrong first ID.

New regression coverage:
`t/atomicint-class-body-static-attribute-default.t`.
