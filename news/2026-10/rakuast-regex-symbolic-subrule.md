# `<::(EXPR)>` keeps its regex tree

A regex that calls a rule by a computed name, `/ <::($name)> /`, had no
node in mutsu's source-level regex tree. The tree parser gave up on the
whole regex, so `.AST` refused it ("literal Regex without a declarator
tree") and so did every grammar token holding one under
`MUTSU_RAKUAST=1`.

The tree now has a `SymbolicSubrule` node with the parsed expression and its
source. It renders as rakudo 2026.09 does: a `Regex::Assertion::Named` whose
name is an empty part followed by an expression part, capturing unless
written `<.::(…)>`, and wrapped in `Regex::Assertion::Alias` for
`<q=::(…)>`. Matching still resolves the name at match time from the regex
text, as it did before. The `<::` spelling was also being read as the start
of a `<:property>` character class, which the atom parser no longer does.

Lowering rebuilds the call's source from the expression. That fallback
printer now handles an interpolated string (`<::("$b")>`), each computed part
written as an interpolated `{…}` block.
