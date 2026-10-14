# Package-qualified variable declarations cross the RakuAST boundary

`my $pkg::x = 7` declares `$x` in a stub package `pkg`. Under
`MUTSU_RAKUAST=1` the lowering refused it, because the declared name is a
multi-part `RakuAST::Name` (`Name.from-identifier-parts("pkg", "x")`, as
rakudo 2026.09 renders it), and the lowering only read a one-part name.

Two related name forms now match rakudo too. The stub package counts as a
type for the rest of the unit, and `OUR::pkg` / `MY::A` over a name that
resolves is a `Term::Name`. Before, the converter rendered it as a
`Call::Name`, which lowered to a sub lookup and failed with "Could not find
symbol '&pkg'".

Looking at this turned up a bug in the ordinary frontend as well:
`constant c = 1; OUR::c` dies looking for `&c` (#13198).
