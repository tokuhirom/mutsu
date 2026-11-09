# A slang can override the action of a core `given`, `for`, `if` or `while`

A slang's actions role may define `method statement-control:sym<given>(Mu $/)` without a
matching token: the Rust parser still parses the statement, the slang host hands the statement
to the method as a `RakuAST::Statement::Given` (the `callsame` answer is that same node), and the
node the method attaches replaces the statement. Before, activation refused the method with "grammar
rule override 'statement-control:sym<given>' is not supported", which took down every
Qwiratry module that loads `Qwiratry::Query::Slang::Topic` (about 35 of its test files).
`for`, `if` and `while` work the same way; statement modifiers are not these productions.
