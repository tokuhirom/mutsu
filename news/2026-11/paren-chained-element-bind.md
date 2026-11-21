# A chained element bind parses in expression position

`%h<a><b> := 1` parsed as a statement, but `say (%h<a><b> := 1)`, `my $v = (%h<a><b> := 1)` and
`(@a[0][1] := (3, 4))` were rejected with "Confused" (#18499): the expression-level `:=` took an
indexed target only when its subscript was applied to a plain variable, a pseudo-stash or a
method call's result. A chained subscript whose every level names a single element now binds
too, into the element its inner subscripts reach (autovivified, as the statement form does).
A slice or Whatever subscript at any level is still left to the statement-level handler.
