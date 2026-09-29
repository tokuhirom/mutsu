# Role method parameters accept well-known compound core type names

A role method's parameter type constraint naming a well-known compound
(`::`-qualified) core type — `CompUnit::DependencySpecification`,
`Distribution::Resource` — was rejected with `Invalid typename '...' in
parameter declaration.`, even though the identical signature on a `sub` or a
class method resolved it fine:

```raku
role R { method m(CompUnit::DependencySpecification $s) {} }; say "ok"
```

`CompUnit::Repository` already worked as a role-method parameter type
because it is genuinely registered as a role (Rakudo's `CompUnit::Repository`
requires `id`/`need`/`loaded`), which made the gap look narrower than it was.

The role-method parameter validator is the only caller strict enough to
notice: it relies on `Interpreter::is_resolvable_type`, which consulted
`is_known_type_constraint` (the allowlist for unqualified builtin names like
`Int`, `Str`, ...) but never its compound sibling `is_known_compound_type`
(the allowlist for `::`-qualified core names like
`CompUnit::RepositoryRegistry`, `Distribution::Hash`, ...). A `sub`'s
signature pre-pass skips validating any qualified name outright, which is
why the same constraint on a `sub` or class method never hit this gap.

`Distribution::Resource` was additionally missing from
`is_known_compound_type` itself (confirmed as a real class via
`raku-doc/type-graph.txt`), so it needed adding there too.

Fixed by wiring `is_known_compound_type` into `is_resolvable_type` and
adding the missing `Distribution::Resource` entry
([#9835](https://github.com/tokuhirom/mutsu/issues/9835)).
