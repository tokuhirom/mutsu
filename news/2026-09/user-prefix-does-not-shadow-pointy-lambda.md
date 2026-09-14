# User-defined minus prefixes no longer shadow pointy lambdas

A `prefix:<->` declaration could consume the leading `-` of a later `-> $x {
... }` pointy lambda. This prevented `Red::Operators` from loading because it
declares that prefix and uses pointy lambdas in method arguments.

The parser now reserves the complete `->` spelling for pointy lambdas before
matching user-defined prefix operators. Ordinary user-defined minus prefixes
continue to work, and the Red-shaped map expression is covered by a regression
test.
