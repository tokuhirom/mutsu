# `proto rule` and `proto regex` are proto regexes, not proto subs

A role carrying a `proto rule` could be instantiated once. The next instantiation (a
second role declaring the same proto name, or the same role punned with `.new` and then
composed into a grammar) died:

```
Could not instantiate role 'RB' because it died with X::Redeclaration
(Redeclaration of routine 'operation'. Did you mean to declare a multi-sub?)
```

This was the largest non-parse, non-timeout cluster in the ecosystem ledger
([#9337](https://github.com/tokuhirom/mutsu/issues/9337)), with 9 distributions:
Grammar::Common, Red, HTML::Component and six DSL::* ones. The DSL::* distributions
compose shared grammar roles into many grammars.

The cause was in the parser (`proto_decl_scoped`, `src/parser/stmt/class/package_decl.rs`).
Only `proto token` became a `Stmt::ProtoToken`. `proto rule` and `proto regex` fell through
to `Stmt::ProtoDecl`, which is a proto **sub**. Running the role body therefore registered a
package-level routine `GLOBAL::operation`, and the second run of the body found it and
reported a redeclaration. `proto token` never had the problem, and the three spellings now
parse the same way.

Rakudo also refuses a duplicate proto regex in one package body at compile time
(`Package 'C' already has a regex 'f'`), including a `proto token` next to a plain `token`
of the same name. The redeclaration scan for EVAL'd code
(`src/runtime/system_eval_redecl.rs`) only counted `token` declarations, so it now counts
`rule`s and proto regexes in the same namespace. `t/oo/role/role-proto-diamond.t` pinned
the old behaviour, where the duplicate was caught only when the role body ran at
composition (`X::Role::Instantiation`). It now checks rakudo's compile-time message.

Pinned by `t/grammar/role-proto-regex-reinstantiate.t`.
