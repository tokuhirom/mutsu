# Grammar argument expressions keep their call-time bindings

The RakuAST frontend retains named capture references and nested named
arguments when it reconstructs regex argument expressions. A rule no longer
runs those expressions while rendering its pattern: captures, side effects
and exceptions belong to the subrule call actually reached by the cursor.
Existing bound-parameter specialization only substitutes the parameters.

The compiled argument evaluator constructs a Capture through the ordinary
compiler, preserving named arguments, positional Pair data and argument
flattening. Positional Pairs carrying objects remain in the callee's binding
window across backtracking. Constructed Grammar nodes accept their bodies,
and constructed regex/token/rule declarations retain signatures and scopes.
Both parsed and hand-built nodes use the same compiler and regex engine.
