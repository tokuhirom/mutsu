# RakuAST: `grammar G:ver<..>` keeps its adverbs; anonymous `%` / `@` parameters spell their sigil

Two `.AST` / `.DEPARSE` leftovers of #14845, found by diffing a fresh sample of `t/`
files against rakudo.

- `grammar G:ver<1.0>:auth<me> { ... }` dropped the name's adverbs entirely (a class,
  role, module and package kept them), so `.AST` had no `colonpairs` and `G.^ver` was
  unset. The grammar header now takes `:ver` / `:auth` / `:api` the way a module does.
- An anonymous `%` or `@` parameter with a sub-signature (`-> % (:$c!) { ... }`) leaked
  the parser's internal name (`%__ANON_HASH__`) into `ParameterTarget::Var`; the target
  is just the sigil, as rakudo has it.

Measured and left for later (each is a parser-side spelling the model layer cannot
recover): `.&{ ... }` / `.&( ... )` / `.&$f` (rakudo wraps the callee in
`Contextualizer::Item`; the parser merges them with `.$f`), `.&f()` on the topic,
`::<M>` printed as `::{"M"}`, `my %h{Str} of Int` printed as `my Int %h{Str}`,
`*²(4)` printed as `(* ** 2)(4)`, `use Foo:Time` losing its tag.
