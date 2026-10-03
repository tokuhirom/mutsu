use Test;

plan 4;

# An undeclared subrule in a `|` branch is a fate in the LTM NFA, as in
# Rakudo: the branch ranks by what precedes it, so a longer sibling is tried
# first and the missing method is never called (ASN::Grammar's
# `<value:sym<number>> | <binary-value> | <hex-value> | <id-string>`).
grammar N {
    proto token value {*}
    token value:sym<number> { \d+ }
    token number { <value:sym<number>> | <nope> | <id> }
    token only { <nope> }
    token id { <[a..z]>+ }
}
is N.subparse('0', :rule<number>).to, 1, 'the declared branch wins';
is N.subparse('maxInt'.lc, :rule<number>).to, 6, 'the longer declared sibling outranks the fate';
throws-like { N.subparse('x', :rule<only>) }, X::Method::NotFound,
    'a real call of the missing rule still dies';
lives-ok { N.subparse('ab', :rule<number>) }, 'measuring alone does not die';
