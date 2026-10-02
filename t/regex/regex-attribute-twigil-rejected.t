use Test;

# An attribute variable inside a regex is a compile-time X::Attribute::Regex
# (a regex is a method on the Cursor, not on the enclosing class), in a
# regex literal and in a token/regex/rule declaration alike, whatever the
# enclosing routine. A code block may still use one inside a larger
# expression; only a statement that is a bare attribute is rejected.
# Expectations checked against rakudo (#10544).

plan 13;

sub rejects(Str $code, Str $symbol, Str $desc) {
    throws-like { EVAL $code }, X::Attribute::Regex, :$symbol,
        message => /"Attribute '$symbol' not available inside of a regex"/, $desc;
}

rejects 'class C1 { has $.a; sub f { my token t { $!a } } }', '$!a',
    'token declared in a sub of a class body';
rejects 'my regex r { $!a }', '$!a', 'my regex';
rejects 'my rule r { <$!a> }', '$!a', 'rule with <$!attr>';
rejects 'grammar G1 { has $.a; token TOP { $!a } }', '$!a', 'grammar token';
rejects 'grammar G2 { has @.a; token TOP { @!a } }', '@!a', 'array attribute in a token';
rejects 'class C2 { has $.a; method f { "x" ~~ / $!a / } }', '$!a', 'regex literal in a method';
rejects 'class C3 { has @.a; method f { "x" ~~ / @!a / } }', '@!a', 'array attribute in a literal';
rejects 'class C4 { has $!c; method m() { /<?> { $!c }/ } }', '$!c', 'bare attribute statement in a code block';
rejects 'class C5 { has $!c; method m() { / x { 1; $!c } / } }', '$!c', 'bare attribute as a later statement';

class Ok1 { has $.a = "x"; method f { "x" ~~ / { $*OUT.print("") ; ~$!a } x / } }
ok Ok1.new.f, 'an attribute inside a larger expression in a code block is fine';

class Ok2 { has $.a = "x"; method f { my $a = $!a; "x" ~~ / $a / } }
is ~Ok2.new.f, 'x', 'the suggested lexical workaround';

class Ok3 { has $.a = "x"; method f { "x" ~~ / "$!a" / } }
ok Ok3.new.f, 'an attribute in a double-quoted string inside the regex is fine';

lives-ok { EVAL 'class Ok4 { has $.a; method f { "x" ~~ / <[$!a]> / } }' },
    '$!a inside a character class is literal';
