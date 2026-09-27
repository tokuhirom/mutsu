use Test;

# #9644: every LTM measurement runs the NFA (ADR-0125, ADR-0127), not the
# backtracking matcher under a measurement mode. Expected values checked
# against `raku`.

plan 8;

# A proto's candidates are ranked by their NFAs, which ignore `:ratchet`: the
# path `\w+ 'c'` reaches the `<.ws>` fate at 3, so `a` outranks `b` (prefix 2,
# litlen 2). The ratcheted walker found no such path and ranked `b` first.
grammar R {
    proto token t {*}
    token t:sym<b> { 'ab' { make 'B' } }
    token t:sym<a> { [ \w+ 'c' <.ws> ]? 'ab' { make 'A' } }
    token TOP { <t> .* { make $<t>.made } }
}
is R.parse("abcd").made, 'A', 'proto ranking ignores :ratchet, as the NFA does';

# The `:rule<...>` entry point ranks a prefix that ends in a fate by where
# the fate is: `a` (prefix 3) before `b` (prefix 2).
grammar G1 {
    proto token t {*}
    token t:sym<b> { 'ab' }
    token t:sym<a> { 'abc' {} 'd' }
}
is ~G1.parse("abcd", :rule<t>), 'abcd', ':rule<> ranks a fate-ended prefix by its fate';

# A lexical regex is called at run time: a fate, not inlined.
my regex three { 'abc' }
is ~("abcd" ~~ / [ 'ab' | <&three> ] /), 'ab', 'a lexical regex call is a fate';

# A plain method of the grammar is a fate; a builtin assertion is measured.
grammar G2 {
    method meth { self }
    token TOP { [ 'a' <.meth> 'bcd' | 'ab' ] }
}
is ~G2.subparse("abcd"), 'ab', 'a method call is a fate';
grammar G3 {
    token TOP { [ 'a' <.alpha> 'cd' | 'ab' ] }
}
is ~G3.subparse("abcd"), 'abcd', 'a builtin assertion is measured through';

# A subrule's arguments are ignored: it is inlined by name.
grammar G4 {
    token two($x) { 'abc' }
    token TOP { [ 'ab' | <two(1)> ] }
}
is ~G4.subparse("abcd"), 'abc', 'a subrule with arguments is inlined';

# `<::(...)>` names its rule with user code: a fate.
grammar G5 {
    token x { 'abc' }
    token TOP { [ 'ab' | <::("x")> ] }
}
is ~G5.subparse("abcd"), 'ab', 'an indirect subrule call is a fate';

# An `:ignoremark` rule inside a branch is measured over the stripped subject.
grammar G6 {
    token m { :m 'abc' }
    token TOP { [ 'ab' | <m> ] .* }
}
ok G6.parse("äbcd")<m>:exists, 'an :m rule is measured over the stripped subject';
