# An undeclared lowercase bareword is a compile-time error in expression position

`say bar;`, `my $x = bar;` and `say(bar)` used to print `bar` and exit 0. The undeclared-routine
scan now treats a lowercase `Expr::BareWord` in any expression position like the statement form,
so rakudo's `Undeclared routine: bar used at line N` is reported before the program runs.
