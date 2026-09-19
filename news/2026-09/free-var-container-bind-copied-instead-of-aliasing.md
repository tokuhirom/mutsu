# A free-variable `@`/`%` `:=` inside a named sub aliases again

A whole-container `:=` written **inside a named sub** against two outer-scope
arrays left the two names on two *different* containers holding the same values:

```raku
my @src = 1, 2, 3;
my @dst;
sub bind-them() { @dst := @src }
bind-them();
@src[1] = 20;
say @dst[1];          # rakudo: 20      mutsu: 2
@dst[2] = 30;
say @src.join(',');   # rakudo: 1,20,30 mutsu: 1,20,3
```

The same binding written at file scope aliased correctly, and the scalar
spelling of this one did too; only `@`/`%` diverged (#8759).

## What was wrong

A `:=` against a free variable is routed through `OpCode::SetGlobal`, whose
shared-`ContainerCell` branch — the thing that makes two names one container —
excluded `@`/`%` alongside `&`. Such a bind fell through to
`pending_alias_bind_names` instead, which `resolve_pending_alias_binds` turns
into a one-shot value copy plus a bidirectional `local_bind_pairs` entry.

A bind pair is a **scalar** propagation mechanism. Its only consumers copy a
value from one local slot to another on a *whole-variable* store; an element
store mutates the backing node and writes no local slot at all, so nothing
propagated. And because the copy handed the target a second `Gc` handle that the
first COW mutation detached, the two names drifted apart permanently. The
`SetLocal` twin already did the right thing for the same source text
(`vm_var_assign_set_local.rs`, the `val_is_container` branch), so the two paths
disagreed about what a whole-container bind means.

## The fix

`SetGlobal` takes the shared cell for `@`/`%` too, gated on the same
`val_is_container` test its `SetLocal` twin uses, and propagates the source
container's declared element/key type to the bound name the same way (`my Int
@t; my Cool @b := @t` ⇒ `@b.of` is `Int`). `&` stays excluded: a sub bind is not
a container bind.

Making that branch reachable exposed a second, older problem underneath it. A
by-name write to a compunit / mainline file-scope lexical (ADR-0024) is stored
*through* whatever `ContainerRef` cell that store already holds for the name, and
for a bind the stored value is itself the bind's new cell — leaving a cell nested
inside a cell. That nesting is not incidental: it is how the holders of the old
cell follow the rebind, a caller frame's own local slot above all, which the
binding routine cannot reach. But every reader dereferenced exactly one level, so
it handed the *inner cell* on where the contained Array was wanted:
`@q.elems` answered 1 (a cell holds one thing) and `@q.push` died with `No such
method 'push' for invocant of type 'Array'`. Only the degenerate self-nesting
form of that had been guarded, by #8048.

So `Value::with_deref` / `Value::into_deref` now collapse a chain of cells to the
value at its end rather than stopping at the first. A cell whose content is
itself a cell is never a value anyone means to see, so this changes nothing for
any other shape. `exec_array_push_op`, which locks the cell by hand instead of
going through the deref family, collapses the chain the same way.

That also fixed a pre-existing scalar divergence nobody had filed: `sub b() { $q
:= $p }` followed by `$q.elems` counted the container (1) instead of the array it
held.

## Pins

`t/vm/writeback/container-bind-free-var-aliases-source.t` (new, 27 assertions
measured against raku v2026.07) covers the array and hash shapes, element stores
and mutating methods in both directions, `=:=` identity, a typed source, an
*empty* source (where a snapshot implementation looks right at bind time and
diverges on the first mutation), a three-name chain, the scalar twin, and — as
the counterweight — that the shared cell still does not leak into an intervening
caller's same-named lexical.

`t/vm/writeback/scalar-bind-unrelated-element-store-semantics.t` §4 had been
asserting only the container-independent half of this shape with a comment
pointing at the issue; its aliasing assertions are restored.

One divergence is left and predates this work: an **undeclared** target reports
the adopted `.of` but does not enforce it under its own name, because the element
store consults the name-keyed constraint and nothing registers one for it. A
target with a declared type of its own (`my Cool @b := @typed`) adopts and
enforces `Int` correctly, and is pinned.
