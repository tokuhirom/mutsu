# Two packages cannot declare the same nested type name

```raku
module A1 { class N::C { method tag { 'a1' } } }
module A2 { class N::C { method tag { 'a2' } } }
say A1::N::C.new.tag;
```

raku prints `a1`; mutsu dies at declaration time with

```
Redeclaration of symbol 'C'.
```

The two are distinct types — `A1::N::C` and `A2::N::C` — so there is nothing to
redeclare. The check that rejects it looks up the *last component* of the
declared name (`C`) in a symbol table that is not package-aware for nested names,
so any second `X::C` anywhere in the compilation unit collides with the first.

## Not the same bug as the registration one

The registry-key half of this — `module M { class A::B { } }` registering under
the bare `A::B`, which left `M::A::B.new` unable to find its own ClassDef and
leaked `A::B` into `GLOBAL` — is fixed
(`news/2026-07/nested-type-name-qualified-by-package.md`,
`t/nested-type-name-in-package.t`). This one reproduces both before and after
that change, because it fires in the declaration-time symbol check rather than in
`exec_register_class_op`.

## Why it is worth fixing

It is a hard error, not a wrong answer, so any distribution that declares a
common nested name in more than one of its modules fails to load outright.
`Foo::Cache`, `Foo::Error` and friends are ordinary names for a library to reuse
across sibling packages.

## Where to look

The registration path is `exec_register_class_op` (`src/vm/vm_typedecl_ops.rs`),
which now computes a package-qualified `qualified_name`. The redeclaration
diagnostic is raised earlier, from the declaration-time symbol bookkeeping —
grep for `Redeclaration of symbol` — and needs to key on the same qualified name
rather than the bare final component.

## Repro

The one-liner above, or `tmp/gnlib/GN.rakumod` + `tmp/gn3.raku` for the
module-file shape.
