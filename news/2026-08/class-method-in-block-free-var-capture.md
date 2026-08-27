# A method of a `class` declared inside a block now captures the block's free variables

A variable referenced only from inside a method body of a `class`/`role`
declared inside a block was never added to that block's captured closure env, so
it read as uninitialized when the block was later invoked as a `Callable` value:

```raku
my $l = 42;
my &blk = {
    my class Foo {
        method go() { say "l=$l" }
    }
    Foo.new.go;
};
blk();
```

`raku` prints `l=42`; mutsu warned `Use of uninitialized value element of type
Any in string context` and printed `l=`. This is the class/method sibling of the
nested-named-`sub` gap fixed in
`news/2026-08/nested-named-sub-free-var-capture.md`, and was filed at the time as
`todo/tickets/method-in-class-declared-inside-block-misses-outer-var.md`.

## Root cause

Two independent defects had to be fixed; only the first was the one the ticket
predicted.

### 1. `compute_free_vars` was not idempotent under upvalue promotion

`CompiledCode::compute_upvalues` promotes a read-only free scalar by rewriting
its `GetGlobal(name)` op into `GetUpvalue { index, name_idx }`.
`compute_free_vars` builds its `free` set by scanning ops through
`op_name_const_idx`, which listed the whole `GetGlobal` family but *not*
`GetUpvalue`. So a **second** `compute_free_vars` pass over already-promoted ops
found no name-bearing op for the promoted variable at all and silently reset
`free_var_syms` to empty, while `upvalue_syms` still named it.

That second pass is exactly what `Compiler::compile_method_body` does: it calls
`cc.compute_needs_env_sync()` (which re-runs `compute_free_vars`) on a
`CompiledCode` that `compile_routine_closure_body` had already promoted. Every
class/role method body therefore ended up with an **empty** `free_var_syms` — a
fact that had been invisible because the surviving `free_var_writes` (write ops
are never rewritten) was all the existing consumer,
`type_body_written_lexicals`, needed.

`GetUpvalue` is now part of `op_name_const_idx`, making the free-var scan
idempotent. This is a general soundness fix, not specific to methods: any future
recomputation after promotion would have hit the same silent reset.

### 2. A method body had no channel into the declaring block's capture set

Even with a correct `free_var_syms`, nothing carried it outward. A method is
installed into its type's method table by `RegisterDecl` and — exactly like a
nested named `sub`, and unlike a nested anonymous closure — has no runtime
closure-creation op, so it never lands in `closure_compiled_codes` and the
enclosing scope's own free-var scan cannot see it. The pre-existing
`note_type_body_written_lexicals` runtime lane only tracks lexicals a type body
*writes* (which is why the write cases already worked) and never populates the
capture set a block value carries.

The named-sub fix's `named_sub_free_reads` field was the right channel, so it was
generalized rather than duplicated: it is now
`CompiledCode::nested_routine_free_reads`, "the free-variable set of each
directly-nested *registered routine*", with two producers —
`compile_sub_body_with_deprecation` (unchanged behaviour) and
`compile_method_body` / `record_type_body_captures_uncompiled` (new). The fold in
`compute_free_vars` is unchanged.

### 3. Declaration-time default expressions are a third capture site

A method parameter's default (`method go($x = $l)`) and an attribute's default
(`has $.a = $l`) are evaluated from AST / from a standalone `CompiledDeclExpr`
chunk at call and construction time respectively. Neither contributes ops to the
method body's `CompiledCode`, so the fold above could not see them either.
`Compiler::bubble_decl_time_free_reads` now feeds both into the same channel:
parameter defaults and `where` constraints are harvested in
`compile_method_body` via a throwaway analysis compile, and attribute
defaults/`where` constraints are read straight off the already-compiled chunks in
`add_class_decl_plan` / `add_role_decl_plan`. Both harvests are filtered to plain
user lexicals, since a standalone chunk owns no locals and so makes every name it
touches look free.

## Verification

`t/closure-capture-class-in-block-method.t` pins 21 cases, all cross-checked
against `raku` v2026.06: scalar/array/hash reads, an outer lexical `Callable`, a
`role` composed into a class, `submethod`, `multi method`, a private `method
!p()`, two block levels of nesting, a parameter default, an attribute default,
`BUILD`, `TWEAK`, a write, a read-modify-write, and five negative controls
(`self`, `$.attr`/`$!attr`, a shadowing parameter, a shadowing `my`, and the
implicit `%_`) that must NOT be pulled in as captures. Twelve of them failed
before this change.

Two masking effects made an earlier draft of that file vacuous, and are recorded
in its header so they are not reintroduced: wrapping a case in a bare `{ ... }`
scope block routes it through `OpCode::BlockScope`'s conservative env-sync gate,
and — new to this file — a single top-level named `sub` declaration anywhere in
the compilation unit makes the mainline sync its locals into the name-keyed env
wholesale, which papers over every case in the file. The test file therefore
keeps each case flat and declares no top-level named `sub` at all.
