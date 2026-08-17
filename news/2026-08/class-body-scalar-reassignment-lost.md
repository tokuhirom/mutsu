# Class-body scalar reassignment by a later statement is now visible to methods

A plain reassignment of a class-body `my` scalar by a LATER body statement
never landed in the store methods close over:

```raku
class C {
    my $x = 10;
    $x = 20;
    method x { $x }
}
say C.x;   # raku: 20, mutsu (before): 10
```

## Root cause

Each class-body statement compiles as its own chunk under the class package
(`compile_decl_stmts_chunk_in_package`), so a sibling statement's write to a
`my` lexical is name-mediated through `env` and the name forms are
asymmetric: the declaration flushes the local to env under the bare
sigil-less name (`"x"`), while a later plain reassignment compiles via
`emit_set_named_var`, which package-qualifies free-standing names
(`SetGlobal("C::x")`).

`persist_class_body_statics` (`src/runtime/registration_class_body_exit.rs`),
which copies class-body `my` statics into `package_lexicals[C]` for methods
to read, only ever looked at the bare env key — so it copied the stale
declaration-time value (10) and never saw the reassignment (20).

Worse, its existing "skip `our` package vars" guard
(`self.get_our_var(&qualified).is_some() || self.env.contains_key(&qualified)`)
positively dropped the name from the static store whenever a same-shaped
qualified key existed — which is *always* true after a reassignment, because
the general `SetGlobal` handler (`vm_exec_dispatch.rs`) unconditionally
mirrors every package-qualified write into the `our_vars` store (not only
genuine `our` declarations — it exists so package-qualified access via
`::('name')` and similar mechanisms stays coherent). So the static was
silently excluded from `package_lexicals` entirely, and the method fell back
to a stray leftover bare-name env entry instead.

## Fix

`persist_class_body_statics` now checks `declared_statics` first: for a name
this body's own `my`/`state` genuinely declared, it prefers the value found
under the qualified key (checking both the `our_vars` store and `env`) over
the bare declaration-time snapshot. Non-declared names (a real leaked `our`
package var) keep the original skip-entirely behavior.

Pinned by a new case in `t/class-body-lexical-read.t` (the write-side twin of
the existing hash/array class-body-my read fix).
