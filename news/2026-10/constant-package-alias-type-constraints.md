Constant package aliases now resolve nested types in variable, parameter,
return and container constraints. For example, after `constant VT = Foo::Tree`,
`my VT::Node $node` checks against `Foo::Tree::Node`, using the same package
alias resolution as a type-object expression. Definedness constraints and
later assignments retain the resolved type check.
