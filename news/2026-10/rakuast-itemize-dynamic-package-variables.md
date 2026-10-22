# RakuAST itemizes dynamic and package variables

`Contextualizer::Item` now lowers array and hash variables represented by
`Var::Dynamic` or `Var::Package`, as well as lexical and anonymous variables.
Expressions such as `$%*ENV` and `$%Package::hash` round-trip through the
RakuAST frontend and keep the existing itemization behavior.

The focused dual-oracle test covers the node shape and EVAL of dynamic,
package, and lexical variables. `roast/S02-magicals/env.t` exercises the
dynamic hash form in the RakuAST frontend.
