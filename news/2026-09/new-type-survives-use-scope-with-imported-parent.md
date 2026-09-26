# A type minted inside a `use`-containing routine keeps its imported parent

Red's `create-resultseq` builds every model's `ResultSeq` class like this:

```raku
sub create-resultseq($rs-class-name, Mu \type) is export is raw {
    use Red::DefaultResultSeq;
    my $rs-class := Metamodel::ClassHOW.new_type: :name($rs-class-name);
    $rs-class.^add_parent: Red::DefaultResultSeq;
    ...
    $rs-class
}
```

A routine whose body has a `use` opens an import scope, and closing it rolls
the class registry back so the imported names stay lexical to the routine.
The rollback kept only classes registered before the scope plus
`A::B`-qualified ones, so it dropped two things it had no business dropping:
the type `new_type` had just minted (it was not there before the scope), and
the imported bare-named class the new type now inherits from. The caller got
back a type whose definition was gone -- `^parents` empty, `.new` dying with
`X::Method::NotFound`, `^mro` missing the parent.

The rollback now also keeps every type minted by `new_type` (recorded in a new
never-rolled-back `persistent_classes` set -- a run-time-created type is not a
lexical import) and, transitively, every class a kept class names as a parent.
The imported *name* still stays lexical in every case where nothing escaping
depends on it. Pinned by `t/vm/scope/add-parent-lexically-imported-class.t`
(#9532, part of #7988).
