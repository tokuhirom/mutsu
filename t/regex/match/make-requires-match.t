use Test;

plan 5;

# `make` outside of a successful match: $/ is Nil, so it must throw
# (Type/Match.rakudoc, `make`).
try { make "x"; CATCH { default {
    like .message, /'The make function expects $/ to contain a Match, but it contains Nil'/,
        'make with Nil in $/ dies';
} } }

grammar G { token TOP { a } }
class A { method TOP($/) { make "x" } }
is G.parse("a", :actions(A)).made, "x", 'make inside an action method works';

"a" ~~ / a { make 5 } /;
is $/.made, 5, 'make inside a regex code block works';

"b" ~~ /a/;
throws-like { make 1 }, Exception, 'make after a failed match (Nil in $/) dies';

"a" ~~ /a/;
lives-ok { make 2 }, 'make after a successful match lives';
