# A nested sub shares its routine's own `@`/`%` from the routine's entry

`sub mk2 { my @u = 'mk2'; my $r = acc(); sub acc { @u.join }; $r }` returned
the program's `@u` when a program-scope `sub acc { @u.join }` with the same body
existed (#13890). The two declarations share one compiled body, so #13892's
per-body owner check could not tell them apart. The nested `acc` had no alias
for `@u` yet, because the hoisted registration seeds a cell only for a scalar.
Its read then fell to the program sub's captured cell.

The hoisted registration now also seeds a cell for the routine's own
still-empty `@`/`%`/`$` variable (a `fresh_local` slot, which a call no longer
fills by name). When the declaration runs, it gives its value to that cell and
the slot goes back to it, so the routine and the nested sub share one
container from the routine's entry. A call before the declaration sees an
empty array, as in rakudo. The declared element type moves with the value.
