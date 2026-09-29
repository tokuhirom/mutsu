use Test;
plan 3;

# `make` attaches its value to `$/` (Rakudo: `$/.made`), so it needs a
# successful match in `$/` first; outside one it throws (see
# t/regex/match/make-requires-match.t).
"a" ~~ /a/;
make("alpha");
is $/.made, "alpha", 'make sets $/.made after a successful match';

make(42);
is $/.made, 42, 'a second make replaces the made value';

"b" ~~ /b/;
nok $/.made.defined, 'a new match starts with no made value';
