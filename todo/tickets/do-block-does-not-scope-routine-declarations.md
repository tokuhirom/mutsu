# A `do { ... }` block does not scope the routine declarations inside it

A routine declared inside a value-position `do { ... }` block leaks into the
enclosing scope and permanently replaces a same-named outer routine. The
statement-form bare block `{ ... }` gets this right; only the `do` form does not.

Measured against Rakudo v2026.06 on 2026-09-04:

```raku
sub foo() { "outer" }
my $inner = do { sub foo() { "inner" }; foo() };
say $inner;   # raku: inner   mutsu: inner
say foo();    # raku: outer   mutsu: inner   <-- WRONG
```

The statement form is correct today:

```raku
sub foo() { "outer" }
my $inner;
{ sub foo() { "inner" }; $inner = foo(); }
say $inner;   # inner
say foo();    # outer   <-- correct in both
```

This is not specific to `multi`/`proto` — a plain single `sub` leaks the same way,
and so does the whole proto/candidate family (an inner `proto`+`multi` inside a
`do` block keeps answering calls made after the block).

## Root cause

The block-scope machinery that makes lexical routine declarations work is the
routine-registry snapshot/restore pair `snapshot_routine_registry` /
`restore_routine_registry` (`src/runtime/accessors_misc.rs`). `OpCode::BlockScope`
takes it unconditionally (`src/vm/vm_misc_scope.rs`, around the
`routine_snapshot` binding), and so does every routine call and every for-loop body
that declares routines (`src/vm/vm_for_loop_dispatch.rs`, gated on
`Compiler::stmts_declare_routines`).

`OpCode::DoBlockExpr` takes none of it. `Compiler::compile_do_block_expr`
(`src/compiler/helpers_do_expr.rs`) emits `DoBlockExpr { scope_isolate: false, ... }`
and then `compile_block_inline`s the body straight into the enclosing code; the VM
side (`src/vm/vm_misc_block.rs`) only saves/restores `env` when `scope_isolate` is
set, and the registry is never involved either way. So the body's `RegisterDecl`s
write into the enclosing scope's registry and nothing puts the outer entries back.

## Why this is more than a one-liner

The obvious fix is to snapshot/restore the routine registry around `DoBlockExpr`
when the body declares routines — `Compiler::stmts_declare_routines` already exists
for exactly this decision, and the for-loop path is the precedent to copy. But:

- it needs a new compile-time flag on `OpCode::DoBlockExpr` (watch the 48-byte
  `opcode_size_guard`; the variant currently has spare padding), and
- `do { ... }` is everywhere in the corpus, so any routine that a `do` block
  currently leaks into an enclosing scope and that something later calls will stop
  resolving. That is the correct semantics, but the blast radius wants a full
  roast + batteries run, not a targeted sweep.

## Repro files

The four-line repro above. `t/multi-proto-lexical-scope.t` deliberately uses
statement-form bare blocks with an outer `my $x` instead of `do { ... }` because of
this bug; when it is fixed, that test can be simplified back to the `do` form (raku
passes both shapes).
