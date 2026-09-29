use Test;

plan 9;

# `make` outside of a successful match: $/ is Nil, so it must throw
# X::Make::MatchRequired (Type/Match.rakudoc, `make`).
throws-like { make "x" }, X::Make::MatchRequired,
    message => 'The make function expects $/ to contain a Match, but it contains Nil',
    'make with Nil in $/ dies';

grammar G { token TOP { a } }
class A { method TOP($/) { make "x" } }
is G.parse("a", :actions(A)).made, "x", 'make inside an action method works';

# The action's `$/` is its parameter: a nested failed regex op must not make
# `make` see a Nil `$/`.
grammar G2 { token TOP { <thing> }; token thing { \d } }
class A2 {
    method thing($/) {
        my $s = "42";
        $s .= subst(/x/, "");
        make 99;
    }
}
is G2.parse("4", :actions(A2))<thing>.made, 99,
    'make in an action after a failed nested subst uses the method $/';

"a" ~~ / a { make 5 } /;
is $/.made, 5, 'make inside a regex code block works';

"b" ~~ /a/;
throws-like { make 1 }, X::Make::MatchRequired,
    'make after a failed match (Nil in $/) dies';

$/ = 42;
throws-like { make 1 }, X::Make::MatchRequired,
    message => 'The make function expects $/ to contain a Match, but it contains Int',
    got => 42,
    'make with a non-Match in $/ names its type';

"a" ~~ /a/;
lives-ok { make 2 }, 'make after a successful match lives';
make 7;
is $/.made, 7, 'make at top level sets $/.made';

# A user-declared `make` shadows the builtin and needs no Match.
{
    my sub make($x) { $x * 2 }
    is make(21), 42, 'a lexical sub make shadows the builtin';
}
