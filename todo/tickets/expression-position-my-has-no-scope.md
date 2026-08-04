# An expression-position `my` writes the enclosing scope's variable

A `my` declaration written in *expression* position — `(my $p := ...)`,
`(my $x = 1)`, anything the parser turns into `Expr::DoStmt(VarDecl)` — is
compiled env-only: the compiler explicitly does not allocate a local slot for it
("Expression-position declarations are env-only (stored via SetGlobal, no local
slot)", `src/compiler/expr_block.rs`). The store therefore lands on whatever
binding of that name is currently visible, so the declaration is not scoped to
its block at all:

```raku
sub blk(&b) { b() }
my $p = 'outer';
blk { my $v = 100; (my $p := foo => $v).WHICH; };
say $p;      # mutsu: :foo(100)      raku: "outer"

my $q = 'outer';
blk { my $v = 100; my $q := foo => $v; };
say $q;      # both: "outer"   (statement position is correctly scoped)
```

The statement form is fine — it gets a slot — so this is specific to the
expression form. `tmp/pd3.p6` is the repro above.

## Why it is not a one-liner

The obvious fix is to `alloc_local(name)` in the expression-position `VarDecl`
arm so `emit_set_named_var` writes a slot. That arm is large and carries a lot
of behaviour that currently depends on the env-only store: the fresh-container
detach for `(my @o = $_)` in a loop, the `__do_decl_init_*` marker caching for
bare container decls, `is default(...)` trait application, the `SetVarType`
re-tagging for `$(my Int %{Int})` round-trips, and the `__ANON_STATE__`
`WrapScalar` path. Each needs re-checking against a slot-backed store, and
`alloc_local` reuses a same-named slot when shadow slots are off, so the
declaration would silently alias an outer binding in the *same* frame instead of
shadowing it.

## Current mitigation

`CompiledCode::expr_declared_syms` (added 2026-08-04) records these names so the
free-variable analysis does not read the env-only store as "the enclosing
scope's local is captured and mutated by this closure". Without it the escape
analysis promoted the enclosing local to a shared `ContainerRef` cell, and an
unrelated later `my Pair $p` in that scope found the cell instead of its own
fresh binding (roast `S02-types/pair.t` #181). That fixes the cell-promotion
axis only — the leak above is untouched.
