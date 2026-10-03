# Badger's test suite passes: lexical namespaces, `.^parameterize`, grammar errors

All four test files of the Badger distribution (SQL-file-to-sub generator)
now pass under mutsu, through these general fixes:

- **`my class AST::Param` under `my module AST {}`** stays visible in its own
  compilation unit; only a `my class` written *inside* a package body is
  hidden after it.
- **`R.^parameterize($value)`** parameterizes a role by the value, exactly as
  `R[$value]` does, instead of spelling the value into a package name.
- **`my class T { ... }.new(...)`** as a statement is a postfix on the type
  object (and keeps the class lexical), also when its body uses `$!attr`.
- **`<|b>`, `<|c>`, `<|g>` and `<!|w>`** regex boundary assertions.
- **A statement starting with a hash composer and a comma**
  (`method hashes { {result => 1}, {result => 2} }`) is one list, not two
  statements.
- **Grammar exceptions:** the first exception a subrule method or argument
  raises ends the parse — a later `||` branch no longer runs or replaces it —
  and a subrule's argument list (`<.panic: "dup $<name>">`) sees the
  captures the enclosing rule already took. `"$<name>"` in a string now reads
  the capture variable like a bare `$<name>`.
