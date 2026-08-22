# A shadowing declaration in a branch/loop body permanently drops the OUTER typed SCALAR's constraint

## Found while

Fixing the mirror-image bug — `news/2026-08/typed-declaration-shadow-scope-leak.md`
(a typed declaration that shadows an outer binding used to LEAK its constraint
onto that outer binding). While writing the "the fix must not disable
enforcement where it belongs" half of `t/typed-constraint-shadow-scope.t`, three
rows turned out to be broken in the OTHER direction, and to have been broken all
along: they reproduce **identically before and after** that fix (verified by
reverting `src/` to `main`, rebuilding, and re-running the probe below).

## Repro

`raku` dies on every row. mutsu lives on A2/A3/A4.

```raku
sub t($label, &c) {
    my $r = "lives";
    try { c(); CATCH { default { $r = "DIES" } } }
    say "$label: $r";
}

# A1: baseline, no inner declaration at all
t("A1", { sub f { my Str $x; $x = 42 }; f() });                                    # DIES (correct)
# A2: inner UNTYPED shadow in a branch
t("A2", { sub f { my Str $x; if True { my $x = 1; }; $x = 42 }; f() });            # lives (WRONG)
# A3: inner TYPED shadow in a branch
t("A3", { sub f { my Str $x; if True { my Int $x = 1; }; $x = 42 }; f() });        # lives (WRONG)
# A4: inner TYPED shadow in a loop body
t("A4", { sub f { my Str $x; for 1..1 { my Int $x = 1; }; $x = 42 }; f() });       # lives (WRONG)
# A5: the same shadow in a genuine `{ ... }` block
t("A5", { sub f { my Str $x; { my Int $x = 1; }; $x = 42 }; f() });                # DIES (correct)
# A6: a branch that declares something else entirely
t("A6", { sub f { my Str $x; if True { my $q = 1; }; $x = 42 }; f() });            # DIES (correct)
```

Mainline behaves the same as the routine rows. The container twin is **correct**
on every row:

```raku
sub f { my Str @a; if True { my Int @a; }; @a.push(1) }; f()   # DIES, as raku does
```

## Why the container half works and the scalar half does not

This is ADR-0042's own thesis showing through. A container's constraint rides on
the VALUE (`ArrayData`/`HashData` carry `value_type`/`key_type`/`declared_type`),
so restoring the shadowed outer container's value at branch exit restores its
enforcement with it. A scalar has nowhere to put one:
`ValueRepr::ContainerRef(Gc<Mutex<Value>>)` is a bare cell, so a typed scalar's
`of` lives only in the name-keyed `__mutsu_type::<name>` / `var_type_constraints`
lanes — and the shadowing inner declaration is what displaces it.

Note A2: the inner declaration is **untyped**, so no `SetVarType`/
`SetVarTypeScoped` op is emitted for it at all, and the shadow-scope metadata
save added by the fix above never runs. Whatever clears the outer's constraint
therefore sits on the plain `my` declaration path (`SetVarDynamic` /
`SetLocalDecl` / `exec_set_local_op`), not on the type-constraint op — that is
the first thing to locate. `rust-gdb -batch` breakpoints on the
`var_type_constraints`/`__mutsu_type::` removal sites in
`src/runtime/runtime_var_meta.rs` (`set_var_type_constraint_impl`'s clear,
`bind_param_type_constraint`'s `None` arm) will name the culprit without a
rebuild. A5 succeeding is the useful contrast: `exec_block_scope_op`'s full
env restore puts the outer's metadata back, so the clear IS undone there — which
suggests the branch/loop path's targeted restore is simply missing whichever key
the clear touched (possibly the global map, which `pop_loop_local_scope` does not
restore at all).

## Why this is deep, and where it belongs

ADR-0042 §5.2 already owns it: slice 2 gives the scalar container a constraint
field so a typed scalar's `of` travels with the value exactly as
`ArrayData::value_type` does. Once it does, a shadowing declaration cannot
displace it — the outer cell keeps its own `of` — and this divergence closes
structurally, the same way the container half is already closed. Patching the
name lanes instead would grow exactly the by-name mechanism that ADR is deleting,
and A2 shows the patch would have to cover the untyped-declaration path too.

So: **do not fix this by adding another name-keyed save/restore.** Either land
ADR-0042 slice 2, or, if a narrower fix is wanted first, write it up as a slice on
that ADR rather than baking it in — §6 of the ADR explains why the by-name route
is the higher-risk one.

## Pin

`t/typed-constraint-shadow-scope.t`, three `# TODO`-marked `dies-ok` rows
(searching for "ADR-0042 slice 2" finds them). Flip them to plain `dies-ok` when
this is fixed and delete this file.
