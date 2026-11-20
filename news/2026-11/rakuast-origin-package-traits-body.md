# RakuAST package declarations: trait visits, block ranges and adverbs

Package declarations now walk and place like rakudo 2026.11's (#15780):

- `visit-children` of a class, grammar, module or package visits its name
  and its body, and the body `Block` visits the package's traits before its
  `Blockoid`; a role's `RoleBody` visits the role's name, its
  parameterization (an empty `Signature` when none is written) and its
  traits. The `Block` starts at the first trait (`class C is Int { }`), a
  2026.11 change two older tests measured on 2026.09 still pinned.
- A name's adverbs (`class E:ver<1>:auth<zef:x>`) are part of the `Name`,
  each `ColonPair::Value` and its word quote placed, and the traits after
  them are found again.
- A trait's parenthesized argument (`is listarg(1, 2)`), a `handles` term
  (`handles <a>`, `handles *`) and a method's empty `()` signature have
  their ranges.
- A bare `is export` on a module, package or role no longer grows a
  `(:DEFAULT)` argument in `.AST`; only a written tag list is one.
