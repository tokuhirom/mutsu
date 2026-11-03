# A sigilless variable, constant or enum key is a term from its declaration on

Rakudo hoists routines only. `say x; my \x = 1`, `say x; constant x = 1` and
`say x; enum E <x y>` are `Undeclared routine: x`, and so is a use in a nested
block or a routine body that comes before the declaration. mutsu accepted all of
them, because the undeclared-routine scan took every declared name as visible
everywhere in the unit.

`undeclared_routines::check_terms_in_source_order` (ADR-15760 Amendment 1, slice
1b) judges the unit as parsed, before the phasers and the BEGIN prologue move
the declarations to the head: a term is visible in the statement list that
declares it and the lists nested in it, from the declaration on. It runs for the
main program, a freshly compiled module, `EVAL` and the analysis frontend, and
reports only the barewords a term would have excused; everything else stays with
the ordinary check. A term used before it is declared when something else
explains the name (a routine of the same name, a core term such as `e` or `pi`)
is still accepted.

The uppercase enum-key row (`say Y; enum E <Y Z>`) stays open: it needs the
uppercase barewords the frontend carries to be classified (slice 1d).
