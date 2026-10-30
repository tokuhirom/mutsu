# A `use` in an `if` body is lexical to the body

The branch of an `if`/`elsif`/`else` statement is now bracketed by an import region like a bare
block. Before, its `use` bound for the rest of the program and a routine written outside the
block saw the imported type while the body ran (#15698).
