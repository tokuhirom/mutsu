# A slang can override the core `statementlist` method

A grammar role's `method statementlist(|)` is now a wrapper hook (ADR-16888, #16888): the
parser asks the slang host where it would parse a statement list that starts after the `use`,
the host runs the override, and a delegation to the captured core method
(`$grammar.HOW.find_method($grammar, 'statementlist')`, EEL's shape) back-calls the Rust parser
for the core list. The list the override delegates to is not wrapped again; lists nested in it
are. As in rakudo, the list holding the `use` itself is not wrapped. The node the override
answers (a `RakuAST::StatementList`) is lowered where the list stood.
