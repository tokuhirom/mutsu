# A term name before `=>` is a pair key

`ident => value` makes a Pair keyed by the literal identifier even when the
identifier names a declared term — a `sub term:<data-home>`, a sigilless
`my \x`, or an imported value term (Raku's `<?before \h* '=>'>` lookahead).
mutsu parsed such a name as a call of the term, so
`Foo.new(data-home => ...)` passed a positional Pair and died with
"Default constructor ... only takes named arguments". As in Rakudo, only
horizontal whitespace may separate the name from the `=>`.

Found by the ecosystem roulette on XDG::BaseDirectory, whose
`t/006-terms-dynamic.t` now passes under mutsu (all 5 test files pass).
