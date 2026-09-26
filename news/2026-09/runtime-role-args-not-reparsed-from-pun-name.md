# Run-time role parameterisation binds values, not the pun name's text

`$a does R[&f, :$model]` with `$model = "Nope::X"` died with
`No matching candidate found for the parametric role`, while the same call with
`:model<X>` worked. Inside Red (`$attr does Red::Attr::Relationship[&reference,
:$model, :$require, ...]`) the same failure surfaced as a *parse error*,
reported at the importer's position (`-e:1`), which is why the ecosystem ledger
could not locate it.

A run-time `does`/`but` builds a pun class whose name stringifies the argument
values (`R[,model\tNope::X,...]`) and passes the evaluated values along as
literal parent arguments. Composition already preferred those values, but a
heuristic that re-reads the *text* of the pun name to spot type-expression
arguments (`R[Str:D(Numeric)]`) still ran: `model\tNope::X` contains `::`, so it
was classified as a type expression and the whole application fell back to
re-parsing that text as source. Arguments that arrive already evaluated are now
exempt from the text heuristic, so they bind as the values they are.

The ticket's side symptom is fixed too: `R[$v]` with `$v = [5, 7, 9]` passed
`3`, because the itemized-array subscript rule (numify for `@a[$v]`) ran before
the role-parameterisation arm. A role's `[...]` is parameterisation, not
subscripting, so the array is now one argument (`P[Array]`, as in rakudo).

Red's `Red::Migration::Column` / `Red::Migration::Table` now parse and advance
to the independent run-time blockers #9498 and (Table) the attribute lookup
that follows it. A same-named outer `my &f` shadowing a role's `&f` parameter,
found on the way, is #9513. Pinned by
`t/oo/role/runtime-parametric-role-value-args.t` (#9497, part of #7988).
