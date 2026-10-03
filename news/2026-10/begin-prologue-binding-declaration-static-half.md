# A binding declaration gets a static half in the BEGIN prologue

In a unit with a BEGIN-time effect (a `BEGIN`, a `constant`, a `use`), the
unit's routines move into the BEGIN prologue (ADR-0134). An assigned
declaration (`my $x = 1`) already left its static half there, so the routines
were compiled after the name was declared. A binding declaration
(`my $x := 42`) did not. Its whole statement stayed at its position, so a
routine compiled ahead of it never captured the binding. A sub writing
`$imm` while its caller had a same-named readonly parameter then fell back to
the caller's mark: "Cannot assign to a readonly variable or a value" instead
of "Cannot assign to an immutable value" (#11263).

The prologue now declares the name ahead of the routines as well, and the
binding declaration re-declares the same slot at its position. When that
re-declaration replaces the cell a mainline sub captured, the capture follows
the new binding, as an in-sequence registration would have captured it.

Found along the way: `$alias++` in a sub, with `$alias := $src` and a caller
holding a same-named readonly parameter, still dies (#11539).
