# A visible outer declaration makes a later inner one legal

`Illegally post-declared type` fired for a use whose name a nearer statement
list declares further down, even when an outer list already declared it: rak's
`our class Rak { ... }` stub at file level, used inside the routine that later
declares the full `class Rak`. rakudo binds the use to the visible declaration.
A sigilless parameter (`-> \Bla { Bla.new }`) binding the same name as a type
the unit declares later was refused too.

The check now looks outwards for a visible declaration before calling a use
post-declared, and ignores a name a sigilless parameter binds as a term. rak,
App::Rak and App::Rak::Complete now load and their tests match rakudo;
ValueClass gets past this check (it stops at #16389).
