# RakuAST: a third set of roast-corpus origins

The `.origin` differential over a second sample of whitelisted roast files
(#15780) closed these gaps:

- quotes with doubled two-byte brackets (`Q«« a »»`);
- regex property elements (`<:Alpha>`, `<-:Digit>`), with each character
  class element running through the blanks before the next, as Rakudo's do;
- `Int:_` in a declaration, the U+2212 minus as a prefix (`−∞`), and a
  package name ending in `::` as a term (`RT::.WHAT`, `MY::<$j>`);
- a contextualizer interpolated into a regex (`$<cat>=@(%what.keys)`);
- a parameterized `is` trait on a declaration (`has @.b is Array[Int]`);
- a qualified call with parentheses (`Foo::run()`), a phaser with a block as
  an expression (`my $r = INIT { ... }`), `temp $_ = 42`, literal and `*`
  shape dimensions (`my @md[2;*]`), an anonymous variable under a phaser
  (`NEXT $++`) and an optional anonymous parameter (`$?`);
- a stub's `ArgList` (`...`, `!!! "x"`), which Rakudo visits and mutsu did
  not.

The shape differences the sample also showed are filed as #18597 and
#18598.
