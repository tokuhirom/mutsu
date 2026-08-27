# `return-rw` produces first-class containers, for a scalar variable and for a list

`return-rw` handed the caller a *container* only when its operand was a subscript
or an attribute (ADR-0059 Slice 1). A bare scalar lexical still compiled to a
decontainerized read, and a multi-operand `return-rw` was not routed through the
container-producing path at all. Assignment (`f() = v`) papered over the scalar
case with a caller-side re-interpretation of the callee's AST tail, so every shape
that needed the container as a real *value* was broken — and the multi-operand
shape failed **silently**:

```raku
my $v = 1; sub f() { return-rw $v }; my $r := f(); $r = 5; say $v
#   raku: 5     mutsu: "Cannot assign to an immutable value"

sub g() { return-rw my $x = 1 }; my $r := g(); $r = 5; say $r
#   raku: 5     mutsu: "Cannot assign to an immutable value"

my $a = 1; my $b = 2; sub h() { return-rw $a, $b }; (h())[0] = 9; say $a
#   raku: 9     mutsu: 1     <-- no error, the write just vanished
```

All four repros (and the `my @r := h(); @r[0] = 9` variant) now match `raku`, and
so does the subscript control case that already worked.

## What was actually wrong, and what it took

The machinery for "a scalar variable's shared cell" already existed and was
simply never connected to the return path. A List literal `($a, $b)` has boxed
its scalar-variable elements into shared `ContainerRef` cells since the
list-element aliasing work: `WrapVarRef` tags the element with its source name
and `MakeArray` calls `capture_var_cell_inner`, which boxes the variable's own
local slot. Three things were missing.

**1. Production — a bare lexical operand now compiles to its cell.**
`OpCode::CaptureVarCell` is `MakeArray`'s per-element capture spelled as a
standalone opcode: it resolves a `WrapVarRef` tag to the named variable's shared
cell, boxing the slot if it is not one already. `Compiler::compile_return_rw_arg`
emits `WrapVarRef` + `CaptureVarCell` for a plain scalar lexical operand (`$v`,
and `$v.item`, which Raku defines as handing the invocant's container back), and
for an inline declaration operand (`return-rw my $x = 1`) — by that point the
declaration has run, so the same two-op tail boxes the slot it created. The cell
is a GC'd `Gc<Mutex<Value>>`, so it outlives the callee frame; a variable with no
local slot in the frame (an env-resident `my` inside a sub, whose declaration
compiles to `SetGlobal`) has its env entry promoted instead. The capture is
deliberately narrow: `@`/`%`/`&`, twigils, attributes and package-qualified names
are excluded, because their containers are reached by their own machinery.

Sharing is real and bidirectional, not a one-way copy: `f()` twice yields the
same cell, and a later write to `$v` is visible through an earlier binding.

**2. Production — every operand, not just a lone one.** The `args.len() == 1`
gate in `compile_expr_call_inner` is now `!args.is_empty()`, so each operand of
`return-rw $a, $b` produces its own container and `builtin_return_rw` assembles
them into the List it already built.

**3. Consumption — a List element that IS a container is writable.** A List is
immutable as a *container* (its element slots cannot be replaced), but Raku lets
you assign through an element that is itself a container — which is what makes
`my $a = 1; my $l = ($a, $b); $l[0] = 9` write `$a`. The immutable-List arm of
`exec_index_assign_named` already had this exemption for an itemized `Scalar`
element; it now also covers a shared `ContainerRef` cell, writing *through* the
cell so every other alias observes it. That fixed `(h())[0] = 9` and
`my @r := h(); @r[0] = 9`, and also the plain `my $l = ($a, $b); $l[0] = 9`,
which had been dying with "Cannot modify an immutable List (1 2)".

## Two pre-existing leaks this surfaced, both fixed here

**Plain `=` from a container-returning call aliased its source.**
`my @a = 1,2,3; sub e() { return-rw @a[0] }; my $c = e(); $c = 99` wrote `@a` in
mutsu (raku leaves it alone). `=` stores a *value*; only `:=` and rw parameter
binding keep the container. `exec_set_local_op_inner` now dereferences a bare
`ContainerRef` on a non-bind store. Every other producer already deconts at its
read chokepoint (`GetLocal`'s `into_deref`, `resolve_array_entry`, ...), so this
only ever fires for a value that came straight off a call.

**A subscript call argument's `is rw` writeback leaked onto the next call, and
decontainerized its result.** `compile_call_arg_with_escape` queues a writeback
for an `Expr::Index` argument, and exactly one emitter drained the queue — but
not every dispatch shape has a drain point. `ExecCallPairs`, which is what a
listop-style statement call takes (and what `is @q[1], 2, "x"` compiles to), had
none, so its entry stayed pending and was emitted after the **next** call in the
compilation unit. The writeback brackets that call's result with
`SetGlobalRaw`/`GetGlobal`, and `GetGlobal` decontainerizes — so any later
container-returning call silently lost its container:

```raku
use Test;
{ my @q = 1, 2; is @q[1], 2, "x" }
{ my @a = 1, 2, 3; sub e() { return-rw @a[0] }; my $r := e(); $r = 9 }
#   dies: "Cannot assign to an immutable value", in a statement unrelated to the first
```

A call now records the queue depth before compiling its arguments and emits only
what its own arguments queued; the two `ExecCallPairs` sites drop theirs rather
than leave them for someone else. Emitting them *at* those sites was tried first
and is wrong for a different reason: the writeback's `===`/`eqv` skip guards fail
open on values with no stable identity (RakuAST nodes compare `False` against
themselves — filed as `todo/tickets/rakuast-nodes-have-no-stable-identity.md`),
so it fired on `$signature.parameters[0]` and tried to assign back through a
method result. ADR-0059 Slice 3 retires these temps altogether.

## Scope

This is ADR-0059 Slice 2 for the `return-rw` spelling. The other half of that
slice — a bare `is rw` tail with no `return-rw` (`sub f() is rw { $x }`) — still
resolves through the caller-side tail re-interpretation, so `rw_sub_target_expr`
/ `is_explicit_return_rw_target` / `assign_rw_target_expr` / `rw_tail_expr` are
not deleted yet. The two source comments that pointed at ADR-0058 for this (the
map/grep deferred-Seq decision) now point at ADR-0059 and say which half is left.

Pinned by `t/return-rw-container-values.t` (39 subtests, identical output under
`raku`), covering all five repros, `return-rw` of a hash element / a missing hash
key / an attribute / a `state` variable, the assignment and `++`/`op=` forms, and
the cell-invisibility invariant on both the returned value and the source
container (`.raku`, `.gist`, `.Str`, `.elems`, arithmetic, list context,
parameter binding, and that a plain `=` copies).
