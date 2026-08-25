# `return-rw` of a scalar variable or a list does not produce first-class containers

Split off from `todo/tickets/control-return-rw-not-mutable.md` while implementing `return-rw`
(see `news/2026-08/return-rw-mutable-call-results.md`). The assignment, `++`/`--` and `op=`
forms all work now; what remains are the two shapes where the container has to survive as a
*value* rather than being reconstructed at the assignment site.

## Root cause

`Compiler::compile_return_rw_arg` (`src/compiler/expr_call.rs`) compiles a `return-rw` operand
in container-producing mode, but only a **subscript / attribute** operand actually yields a
shared cell (`ContainerRef` / a deferred `HashEntryRef`). A bare `Expr::Var` operand still
compiles to a decontainerized read, and a multi-operand `return-rw` is not routed through that
path at all (`compile_expr_call_inner` gates it on `args.len() == 1`).

Assignment (`f() = v`) papers over the scalar case with a *caller-side tail
re-interpretation*: `assign_named_sub_lvalue_with_values` falls back to
`assign_rw_target_expr`, which re-evaluates the callee's tail expression in the CALLING frame
(`src/runtime/builtins_lvalue.rs`, the `is_explicit_return_rw_target` path). That fallback only
works when there is a syntactic assignment site to re-interpret — so every shape that needs the
container as a real value is still broken.

## Repros

Both work in `raku` and fail in mutsu:

```raku
# A. binding a returned scalar container
my $v = 1;
sub f() { return-rw $v }
my $r := f();
$r = 5;
say $v;           # raku: 5   mutsu: "Cannot assign to a readonly variable (r) or a value"

# ... including a container that outlives its declaring scope
sub g() { return-rw my $x = 1 }
my $r := g(); $r = 5; say $r;    # raku: 5   mutsu: same error

# B. return-rw of several values returns values, not containers
my $a = 1; my $b = 2;
sub h() { return-rw $a, $b }
(h())[0] = 9;
say $a;           # raku: 9   mutsu: 1  (silently writes nothing)

my @r := h(); @r[0] = 9;
say $a;           # raku: 9   mutsu: "Cannot modify an immutable List (1 2)"
```

Note that the subscript operand already works, which isolates the gap precisely:

```raku
my @a = 1,2,3;
sub e() { return-rw @a[0] }
my $r := e(); $r = 9; say @a;    # both: [9 2 3]
```

## Why it is large

Fixing A means making a bare lexical compile to a shared cell whenever it is a `return-rw`
operand, and making that cell outlive the callee frame — the same "universal `ContainerRef`
deref" problem the dual-store / element-itemization work keeps running into. It is not a local
change to `return-rw`: every reader of the returned value has to deref transparently, or the
cell leaks into user-visible output.

Fixing B additionally needs a *list of containers* as a runtime value, so `(h())[0] = 9`
resolves to the element's cell rather than to a copy. mutsu has no representation for that
today; `Value::array` of `ContainerRef`s would have to survive `.raku`/`say`/iteration without
each consumer accidentally decontainerizing or accidentally exposing the cell.

## Affected files

- `src/compiler/expr_call.rs` — `compile_return_rw_arg`, and the `args.len() == 1` gate in
  `compile_expr_call_inner`
- `src/runtime/lvalue_container_return.rs` — the write-through side
- `src/runtime/builtins_lvalue.rs` — `assign_rw_target_expr` / `assign_named_sub_lvalue_with_values`,
  the caller-side fallback this would let us retire for the scalar case
- `src/compiler/expr_data.rs` — the container-mode subscript chain that already works

## Also noted

The stale `see ADR-0058 §Slice 2` comments in `src/runtime/builtins_lvalue.rs` (lines ~542 and
~610) point at the wrong ADR — ADR-0058 is the map/grep deferred-Seq decision. Whoever picks
this up should repoint or drop them.
