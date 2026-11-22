# RakuAST: a fifth set of roast-corpus origins

The `.origin` differential over a third sample of whitelisted roast files
(#15780) closed these gaps:

- a quoted word in an interpolating word quote (`«"$*EXECUTABLE" -e "x"»`);
- an anonymous typed invocant (`method !foo(A:D:)`, `B:D: $x`), whose
  parameter the parser now records a range for. A signature that ends with
  the invocant runs through its `:`, as in Rakudo;
- a chain of method calls interpolated into a string
  (`"$x.ord.fmt('%d')"`), simple quotes in the arguments included. This is the
  new `string_postfix_origins` module, split out of `string_origins`;
- a package-qualified callable regex assertion (`<&D::doo>`);
- a loop under a statement prefix (`start until $x { }`), and the setup of
  a C-style loop there (`do loop (my $i = 0; ...) { }`);
- an indirect name of any mix of simple and `::(...)` parts
  (`&CALLER::($s)::not`).

A negated reduction (`[!after]`, `[!==]`) is now
`MetaInfix::Negate(Infix(...))`, as in Rakudo. `[!=]` and `[!~~]` stay
operators of their own. A metaoperator reduction's operator and the infix it
wraps are placed.

The shape differences the sample also showed are filed as #18620 (`::Array`)
and #18621 (`has R $.r .= new(...)`).
