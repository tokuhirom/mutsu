# RakuAST: origins of feeds, package variables and operators called by name

Three `.origin` gaps closed (#15780):

- a chained feed (`@a ==> map(...) ==> my @out`) places its `Feed` at the
  last arrow, as a list infix places its operator;
- a package variable's `Name` (`$OUR::x`, `$::($n)`) runs from after its
  sigil to its end;
- an operator called by name (`infix:<+>(1, 2)`, `prefix:<!>($x)`) now has
  the `Name` Rakudo gives it, its symbol a word quote in `colonpairs`
  (`name_parts::operator_name`), which is placed between the angle
  brackets; it was one opaque identifier string before.
