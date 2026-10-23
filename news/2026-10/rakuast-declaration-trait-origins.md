# RakuAST package and variable traits carry their origins

`.AST` now gives the following nodes the ranges Rakudo does:

- the `Trait::Is`, `Trait::Does` and `Trait::Hides` of a package header
  (`class A is B does R is rw hides C { }`), and the type or name each names;
- the name and block of an exported package (`class C is export { }`),
  which the parser wraps together with its export registration;
- the type of a variable declaration, `Type::Simple` and `Type::Definedness`
  (`my Int $x`, `has Str:D $.s`), and its `is` traits with their names and a
  parenthesized argument (`my $x is default(3)`, `has $.a is rw is
  required`). `visit-children` now visits a declaration's traits after its
  initializer, as Rakudo does.

The parser folds these clauses into parents, roles, flags and type names, so
they are read back from the declaration's text and checked against the node
each one places; a clause this reading does not cover (a parameterized type,
a trait argument holding a quote) keeps no range.

Two ways a term took the range of a term written in another statement are
gone. A statement the parser adds after a marked one (a copy of an
attribute's default) no longer counts as that statement when the parse's
term notes are matched, and a copy the conversion makes of a term only takes
a range from the statement being converted, where it used to take the first
unused range of the same shape anywhere in the unit (`class B { has $.l = 7
}; my $x = 7` placed the attribute's `7` at the second statement's).
