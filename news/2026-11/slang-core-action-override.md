# Slangs by execution, slice 3: a slang's override of a core action (`INFIX-EXPR`, `PREFIX-EXPR`) runs

A slang whose actions role overrides `INFIX-EXPR` (or `PREFIX-EXPR`), calls `callsame` to get the
node the core action built and `attach`es a rewritten one is no longer refused at activation
(#13369, ADR-15762). After the unit is parsed and before it is finalized, each infix (prefix)
application after the slang's `use` is converted to its RakuAST node, run through the slang host's
composed actions, and replaced by the node the action attached: `10 + 3` with a slang rewriting `+`
into `-` is 7, as on rakudo 2026.09.
