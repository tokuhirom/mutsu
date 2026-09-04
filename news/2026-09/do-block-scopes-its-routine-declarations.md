# A `do { ... }` block now scopes the routine declarations inside it

A routine declared inside a value-position `do { ... }` block leaked into the
enclosing scope and permanently replaced a same-named outer routine. The
statement-form bare block `{ ... }` had always been correct; only the `do` form
was not.

```raku
sub foo() { "outer" }
my $inner = do { sub foo() { "inner" }; foo() };
say $inner;   # raku: inner   mutsu: inner
say foo();    # raku: outer   mutsu: inner   <-- was WRONG
```

It was not specific to `multi`/`proto`: a plain single `sub` leaked the same
way, and so did a whole proto/candidate family.

## Root cause

The block-scope machinery that makes lexical routine declarations work is the
routine-registry snapshot/restore pair `snapshot_routine_registry` /
`restore_routine_registry`. `OpCode::BlockScope` takes it, so does every
routine call, and so does every for-loop body that declares routines.
`OpCode::DoBlockExpr` took none of it: `Compiler::compile_do_block_expr`
emitted it and then `compile_block_inline`d the body straight into the
enclosing code, so the body's `RegisterDecl`s wrote into the enclosing scope's
registry and nothing put the outer entries back.

## Fix

`OpCode::DoBlockExpr` grew a compile-time `scope_routines: bool`, set from the
existing `Compiler::stmts_declare_routines` decision that the for-loop path
already uses, and the VM takes the snapshot/restore pair around the body when
it is set. A body that declares no routine — the overwhelmingly common case —
pays nothing, and the variant still fits the 48-byte `opcode_size_guard`.

`stmts_declare_routines` itself only matched `Stmt::SubDecl`, so a body whose
only routine declaration was a `proto` was invisible to it. It now matches
`Stmt::ProtoDecl` too, which fixes the same leak for the for-loop, map/grep and
non-routine-block paths that share the helper.

## Blast radius

Correcting this *removes* names that used to leak, so anything that called a
`do`-block routine from outside the block stops resolving — which is the raku
behaviour, but it is a behaviour change rather than a pure bug fix. It was
validated against the full local `make roast` (1436 files, 218962 tests, all
green) in addition to `make test`, plus CI's batteries gate.

Pinned in `t/do-block-scopes-routine-decls.t`, which passes verbatim under both
`mutsu` and `raku`: shadow-then-restore for a plain `sub` and for a
`proto`+`multi` family, the no-leak-at-all case, nesting, a routine *value*
that escapes the block still being callable, and the statement-form block that
was already correct.
