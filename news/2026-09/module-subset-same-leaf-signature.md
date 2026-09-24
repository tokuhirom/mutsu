# A package-local subset keeps its identity in exported signatures

`unit module Test::Script` declares `subset Script` and uses it in the
signatures of its exported helpers. mutsu treated the package name
`Test::Script` as the parameter type because it mistook the module's final
name for a class self-reference, so every helper rejected valid script paths.

Type-name resolution now prefers an actual nested type such as
`Test::Script::Script` before applying the class self-reference rule.

Pinned by `t/modules/module-subset-same-leaf-signature.t`, reduced from
`Test::Script` 0.0.4.
