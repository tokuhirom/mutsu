# An imported list-associative chain over a built-in operator reaches its candidates

Range::SetOps exports `multi infix:<(|)>(**@r) is assoc<list>` so that
`(2..7) (|) (8..11) (|) (9..13)` unions Ranges in one call. Since a `use`
reads the loaded unit's operator facts (ADR-17558 slice 2a), the importer
parses that chain as one three-operand call, as rakudo does. The call then
reached only the built-in union: the imported candidates are bound in the
importing block's lexical import set, and the multi-operand path looked them
up by name in the routine registry. The chain now runs with the import set's
candidates in scope, the way a binary built-in operator already did, and the
built-in still answers operands no candidate takes.

An `is assoc` trait alone on a candidate of a built-in operator also keeps the
built-in's precedence: `&infix:<(|)>.prec` is `junctive-or`, as on rakudo,
not the default-infix level of a new operator.
