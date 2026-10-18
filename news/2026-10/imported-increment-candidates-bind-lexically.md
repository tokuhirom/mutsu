# Imported `++`/`--` candidates stay in their block, and elements still increment

A block-scoped `use` of a module that exports `multi postfix:<++>` (or
`prefix:<++>`, or either `--`) candidates now adds them to the operator inside
that block only, like the other built-in operators (ADR-8751 stage 2,
slice 9). Before, such an import made every `$i++` on a plain Int in the block
die with "Cannot resolve caller", a routine written outside the block saw the
candidates, and a closure taken in the block lost them.

With any user increment candidate in scope, `@a[0]++` and `%h<a>++` on a value
the candidates do not take now increment the element in place; they left it
unchanged before (#13836).
