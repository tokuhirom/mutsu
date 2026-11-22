# A role type object's method call puns that role, not a same-named lexical one

```raku
role Ro { method r { "outer-role" } }
{ my role Ro { method r { "block-role" } }; say OUTER::Ro.r }
```

This printed `block-role`; rakudo prints `outer-role` (#18233). Calling a
method on a plain role's type object composes the role's pun class, `Ro\0pun`,
once. When the first such call came from a block that declares its own
`my role Ro`, the pun's `does Ro` was resolved through the running scope at
two points: the class registration's type-capture pass and the role candidate
lookup of the composition. Both found the block's role. From then on the
outer role's pun answered with the block role's methods, after the block too.

A pun class now composes exactly the registry entry it is named after
(`resolve_role_candidate_named`). The issue's type-object checks
(`$outer ~~ Inner`, `Inner.ACCEPTS`, a typed parameter) already gave
rakudo's answers on `main`; the new test pins them as well.
