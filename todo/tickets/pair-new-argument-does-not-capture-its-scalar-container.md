# `Pair.new("k", $x)` does not capture `$x`'s container, so `.value = X` does not write through

## Symptom

The fat-arrow form and the `Pair.new` form are the same construct in raku, and
both alias the scalar they are handed:

```
$ raku -e 'my Int $x; my $p = Pair.new("k", $x); $p.value = 5; say $x.raku'   # 5
$ raku -e 'my Int $x; my $p = ("k" => $x);       $p.value = 5; say $x.raku'   # 5
```

mutsu gets the fat-arrow form right (fixed 2026-09-01, ADR-0036's slice 4
prerequisite 1) but not the `Pair.new` form:

```
$ mutsu -e 'my Int $x; my $p = ("k" => $x);       $p.value = 5; say $x.raku'   # 5   ✓
$ mutsu -e 'my Int $x; my $p = Pair.new("k", $x); $p.value = 5; say $x.raku'   # Int ✗
```

The Pair itself still *prints* `k => 5`, because the write is faked by the
standalone-pair env rebind in `assign_method_lvalue_with_values` — the
compensator ADR-0036 slice 4 wants to delete. So the visible damage today is
limited to the source variable, but it becomes a hard failure the moment that
rebind goes.

An *initialized* scalar works in both forms, which is the tell: what is missing
is the capture of a scalar whose slot currently holds a bare type object.

```
$ mutsu -e 'my Int $x = 1; my $p = Pair.new("k", $x); $p.value = 5; say $x'    # 5   ✓
```

## Root cause

`key => $var` is captured at **compile** time: `compile_binary` tags the RHS
with `WrapVarRef` (`src/compiler/expr_binary.rs`, the `TokenKind::FatArrow` arm
calling `scalar_container_alias_name` + `emit_wrap_var_ref`), and
`MakePair`/`MakeNamedArg` box the named local into a shared `ContainerRef`
(`pop_pair_operands_capturing`, `src/vm/vm_mixin_does_ops.rs`).

`Pair.new("k", $x)` is an ordinary method call. Its argument is compiled as a
plain expression, so nothing tags `$x` and the constructor receives the
dereferenced value. There is no `WrapVarRef` for the constructor to unbox.

## Why it is a ticket and not a one-liner

The fix is not "tag every `Pair.new` argument": the general question is which
method arguments capture their scalar's container, and mutsu already has
machinery for the `is rw` / `\(...)` capture cases. The narrow, defensible
version is to give `Pair.new`'s second positional the same compile-time
treatment the fat arrow gets — recognising the receiver as the `Pair` type
object at compile time — which is a special case that should be justified
against ADR-0021 (Pair namedness) before being written.

## Where to look

- `src/compiler/expr_binary.rs` — the `FatArrow` capture arm, and
  `Self::scalar_container_alias_name`, which is the "is this RHS a plain scalar
  variable?" test to reuse.
- `src/compiler/expr_call.rs` — `emit_wrap_var_ref` / `emit_wrap_var_ref_arg_tag`;
  the latter exists precisely for a consumer that reads its target through a
  different op.
- `src/vm/vm_mixin_does_ops.rs` — `pop_pair_operands_capturing`, which is what
  the constructor path would need an equivalent of.

## Repro to pin when fixed

```raku
{
    my Int $x;
    my $p = Pair.new("k", $x);
    $p.value = 5;
    is $x, 5, 'Pair.new captures its scalar argument as a container';
}
{
    my $a;
    my $b;
    my $pa = Pair.new("k", $a);
    my $pb = Pair.new("k", $b);
    $pa.value = 1;
    is $b, Any, 'two undefined scalars stay two distinct containers';
}
```

Related: `todo/tickets/pair-value-assign-does-not-enforce-immutable-value.md`
(the guard that this must land before), and ADR-0036's slice 4 note, which
records both as prerequisites for deleting the compensator.
