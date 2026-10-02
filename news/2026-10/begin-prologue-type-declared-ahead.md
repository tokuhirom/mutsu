# A nested BEGIN behind a type, package, import or code variable runs at BEGIN time

A `BEGIN` nested in a routine or block used to keep its pre-ADR-0134 handling
whenever its scope declared a type, a package, an import or a `my &code`
variable ahead of it. It then ran only when, and every time, the scope ran, so
`sub f { my class K { }; BEGIN say "b" }; say "m"` printed just `m`. Rakudo
prints `b` then `m`.

Such a BEGIN is now lifted into the unit's BEGIN prologue like the others
(#10394):

- **Imports** are repeated in the lifted body's block, so `sub f { use Test;
  BEGIN ok 1 }` runs `ok` at BEGIN time.
- **Code variables** get a static cell like any other lexical, so
  `sub f { my &g; BEGIN &g = { 7 }; g() }` sees the BEGIN's value on every call.
- **Types and packages** are found on the AST with the typed visitor of
  ADR-0137. A BEGIN that does not name the scope's type is lifted without it. One
  that names it gets the declaration repeated in its block when that is
  unobservable (a body of declarations only). A `my` class is stored under its
  declaration site, so the BEGIN sees the very type the scope declares:
  `sub f { my class K { }; my $t = BEGIN K; $t === K }` is now `True`.

A BEGIN that names a class whose body runs code, or that sits behind a pragma or
an operator code variable, still keeps its old handling (ADR-0134 §7).
