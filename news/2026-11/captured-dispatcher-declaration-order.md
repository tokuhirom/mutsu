# A dispatcher code value keeps its candidates in declaration order

A multi dispatcher captured as a code value (a `&name` an `EXPORT` hook hands
out, a re-import's dispatcher) held its candidates in the order the registry's
hash map happened to list them. Two candidates that tie on narrowness — a
role-typed and a class-typed parameter, Red's `trait_mod:<is>(Red::Attr::Column
$attr, :%column!)` and `(Attribute $attr, :%column!)` — then went to whichever
came first in that order instead of the one declared first, as rakudo and
mutsu's own by-name dispatch decide. Red's `has $.x is indexed is unique` lost
its first trait once a module had imported Red before the program did (Red
`t/095-indexes`, #18043). The dispatcher now sorts its candidates by their
declaration stamp when it captures them.
