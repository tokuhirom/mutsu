# A scoped `my TYPE $x`/`my TYPE @a` declaration that SHADOWS an existing outer binding tags the OUTER value, not just the name

## Found while

Implementing ADR-0042 slice 1 (`docs/adr/0042-type-constraints-belong-to-the-container-not-to-a-name.md`).
Slice 1's four mechanical steps (container-side constraint reads, `state`
container tagging, dropping `@`/`%` from the scoped-opcode sigil exclusion,
and `BlockLocalScope` exit cleanup of `__mutsu_type::`/`__mutsu_hash_key_type::`
keys) all landed and verified green against the ADR's own §2.2 container
matrix (7/7), the §3 alias probe (7/7), and the §3.1 `state` container gap
(2/2) — see `t/typed-constraint-scope-matrix.t` and
`t/state-typed-container-alias.t`. This finding is a SEPARATE, deeper bug
discovered while probing a related but distinct shape: not "declare typed
inside a scope, then declare a FRESH untyped variable after the scope
exits" (the ADR's §2 matrix, now fixed), but "an outer variable ALREADY
EXISTS (untyped) before the inner scope runs, and the inner scope's typed
declaration of the SAME name SHADOWS it, then the branch/loop exits and the
(never re-declared) outer variable is reused."

## Repro

```raku
sub via-if {
    my $x;                      # outer, untyped, declared FIRST
    if True { my Str $x = "a"; } # inner SHADOWS the outer $x
    $x = 42;                    # raku: fine. mutsu: dies (Str constraint leaked)
}
via-if();
```

`raku` accepts this (`$x = 42` succeeds). mutsu throws:
```
Type check failed in assignment to $x; expected Str but got Int (42)
```

The exact same shape reproduces for containers:

```raku
sub via-if {
    my @a;                            # outer, untyped, declared FIRST
    if True { my Int @a; @a.push(5); } # inner SHADOWS the outer @a
    @a.push("x");                     # raku: fine. mutsu: dies
}
via-if();
```

**Verified present on `main` BEFORE ADR-0042 slice 1 too** (checked via
`git stash` back to the pre-slice-1 tree) — this is not a regression
introduced by slice 1, and slice 1 does not change its behavior either way
(confirmed both before and after slice 1's four steps produce the identical
failure).

**Affects `if`/`unless`/`else` branches EQUALLY, not just `while`/`loop`/
`repeat`/`for` bodies.** This is the important correction to the ADR's own
framing: ADR-0042 §2.1 predicted that slice 1's `BlockLocalScope` exit
cleanup (step 4) would turn the `if`/`unless`/`else` SCALAR rows green while
leaving `while`/`loop`/`repeat`/`for` broken (no scope wrapper to hook).
Measured directly (`tmp/tc-scalar-matrix2b.raku` during the slice-1 session,
not checked in): for THIS shadow shape specifically, `if`/`unless`/`else`
leak exactly as much as `while`/`loop`/`repeat`/`for` — step 4 does not
close it for branches either. The two ADR matrices (§2's "fresh-after"
matrix vs. this "outer-first shadow" matrix) are genuinely different bugs
that happen to look similar in prose.

## Root cause

`exec_set_var_type` (`src/vm/vm_var_type_ops.rs`) — the handler shared by
both `SetVarType` and `SetVarTypeScoped` — tags whatever value is CURRENTLY
bound to the declared name in `env` at the moment the type-constraint op
runs:

```rust
} else if let Some(value) = self.get_env_with_main_alias(&name) {
    let info = ContainerTypeInfo { value_type: ..., key_type: ..., .. };
    let tagged = self.tag_container_metadata(value, info);
    self.set_env_with_main_alias(&name, tagged.clone());
    self.update_local_if_exists(code, &name, &tagged);
}
```

For a scalar, the sibling branch just above does the same thing more subtly
(seeds a Nil-valued binding with the type object).

The compiler emits the type-constraint op (`SetVarType`/`SetVarTypeScoped`)
BEFORE the declaration's own `SetLocalDecl`/value-store op (confirmed via
`--dump-bytecode`: `SetVarTypeScoped` at a lower instruction index than the
following `SetLocalDecl` for the same declaration). So when a SHADOWING
inner declaration's type op runs, the inner declaration's OWN value has not
been created yet — `get_env_with_main_alias(&name)` still returns the OUTER
variable's value (env is bare-name-keyed, not scope-keyed), and this code
directly mutates/tags THAT value in place (for a uniquely-owned container,
`tag_container_metadata`'s `Arc::make_mut`/`Gc::make_mut` COW mutates the
existing `ArrayData`/`HashData` rather than cloning it, since nothing forces
a clone at refcount 1).

The corruption is therefore on the **VALUE itself**, not on the name-keyed
`__mutsu_type::`/`var_type_constraints` metadata. This is why ADR-0042
slice 1's fixes — which are entirely about routing metadata READS through
`element_constraint_for`/`container_type_metadata` (the container's own
embedded metadata) instead of the scope-blind name map — cannot touch this
bug: the container's own embedded metadata is exactly what got corrupted.
A metadata-key snapshot/restore fix was prototyped during the slice-1
session (extending `loop_local_saved_env`, the existing shadow-restore
mechanism used by `pop_loop_local_scope`, to also snapshot/restore
`__mutsu_type::name`/`__mutsu_hash_key_type::name`) and empirically verified
to NOT fix this shape — confirming the bug is at the value layer, not the
name layer.

## Why this is large / deep

Fixing it correctly means `exec_set_var_type`'s container-tagging (and the
scalar Nil-seeding) must not run against "whatever is currently in `env`
under this name" when the declaration is about to REPLACE that binding with
its own fresh value — i.e., the tagging needs to happen AFTER the
declaration's own `SetLocalDecl`/store op, against the declaration's own
value, not before it against whatever might still be bound to the shared
bare-name env key. That likely means either:

- Reordering the compiler's emission so the type-constraint op runs AFTER
  the value-store op for a declaration with an initializer (checking every
  emission site — `expr_block.rs`, `helpers_ast_utils.rs`, `stmt.rs`, per
  the callers of `emit_set_var_type` found during slice 1), which needs care
  around the "if value is Nil, seed it now" scalar branch (a bare `my Str
  $x;` with no initializer relies on the CURRENT ordering to seed the type
  object before any read can observe Nil).
- Or giving `exec_set_var_type` an explicit flag/signal for "this constraint
  belongs to a declaration that is about to own a NEW value, don't touch
  whatever's currently bound" — distinguishing a genuine `my TYPE $x`/`my
  TYPE @a` from the other callers of the same opcode (`our`, type
  re-declaration on an existing binding, etc., where tagging the CURRENT
  value is exactly the intended behavior).

Both directions touch a hot, widely-shared VM function and need the same
`raku`-verified matrix (fresh-after AND shadow, for scalars AND containers,
across `if`/`unless`/`else`/`while`/`loop`/`repeat`/`for`) to avoid trading
one leak for a different regression. This is genuinely ADR-0042 slice-2/3
adjacent territory (the ADR's own slice 2 gives scalars a cell-carried `of`,
which may incidentally close the scalar half of this bug as a side effect of
no longer needing `exec_set_var_type`'s Nil-seeding hack at all — worth
re-checking once slice 2 lands) but is NOT itself one of slice 1's four
mechanical steps, and is out of scope for that PR.

## Minimal repro files (not checked in — recreate from the snippets above)

- `tmp/tc-scalar-matrix2b.raku` — per-shape check (`if`/`unless`/`else`/
  `while`/`loop`/`repeat`/`for`), all shadow-then-reuse, dies on every row.
- `tmp/tc-container-matrix2b.raku` — the container twin (`if`/`while`/`for`),
  same result.
