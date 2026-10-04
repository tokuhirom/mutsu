use Test;

plan 4;

# A `{ }` block's `$N` is the in-progress match's own capture, never the
# `$N` an earlier, unrelated match left in the enclosing scope (#11740).
my @seen;
"q" ~~ /(q)/;
"a" ~~ / a { @seen.push: ($0 // "none") } /;
is @seen[0], "none", 'no captures yet: $0 is unset, not the previous match';

"zz" ~~ /(z)(z)/;
@seen = ();
"ab" ~~ / (a) { @seen.push: ($0 // "none"); @seen.push: ($1 // "none") } b /;
is @seen[0], "a", 'own capture $0 is visible';
is @seen[1], "none", '$1 not yet captured is unset, not the previous match';

"q" ~~ /(q)/;
@seen = ();
"a" ~~ / a <?{ @seen.push: ($0 // "none"); True }> /;
is @seen[0], "none", 'assertion block does not see the previous match $0';
