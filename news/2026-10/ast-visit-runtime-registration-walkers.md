# Runtime registration and run analyses moved onto the typed AST visitor

Fifteen hand-rolled `Stmt`/`Expr` walkers in the runtime's registration and
program-start code now implement `crate::ast_visit::Visit` (ADR-0137), so each
one searches every child of the tree instead of the forms its `_ =>` arm
happened to list:

- the compile-time private-method checks (`$o!Owner::m` trust and `self!m`
  existence), moved out of `registration.rs` into
  `runtime/registration_private_access.rs`;
- the undeclared-attribute check (`X::Attribute::Undeclared`);
- the compile-time `END` pre-installation;
- the `$=pod` declarant collection;
- the grep "does the matcher contain `last`?" probe;
- the module scans for exported type markers and `state`-declaring subs;
- the slang `package_declarator` fact collection;
- the static-`require` stub scan of the BEGIN prologue.

The positions the old walkers skipped now behave as in rakudo:

- `$!x` naming no attribute is rejected inside a nested `sub`, a parameter
  default, an interpolated block, a named argument and so on, and `@!x` /
  `%!x` are checked at all for the first time.
- An untrusted `$o!Owner::m` or a missing `self!m` is rejected as a `given`
  topic, in a nested `sub` or a parameter default.
- An `END` inside an interpolated block or a parameter default of a never-run
  statement is installed and runs at exit; a `loop (my $i ...)` variable is
  seeded like any other lexical for a never-reached `END`.
- A documented `sub` inside a block gets its concrete `WHEREFORE`, and a
  nested `multi` candidate no longer shifts the `WHEREFORE` of the later
  top-level candidates.
- A `my class ... is export` declared inside a block of a `unit module` is
  exported.

The deliberate stops are explicit hooks with a comment: a nested class (and,
for the `self!m` and attribute checks, a nested role) is checked against
itself, a `try` body still defers both checks to run time, and the `require`
scan stays out of nested blocks, closures and parameter defaults.

Ten walkers in the same files stay hand-rolled and are now annotated in
`scripts/ast-walkers-baseline.txt`: the statement-tail spine in `run.rs`, the
`.defined` shape recognizer in `types/mod.rs`, the BEGIN-prologue transforms
and one-scope classifiers, and the prelude's mutating marker and top-level
declaration check.
