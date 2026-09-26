use Test;

# #9617: an LTM declarative-prefix measurement inlines a subrule once and ends
# the path at a call to a rule it is already walking (Rakudo's NFA `%seen`).
# Expected values checked against `raku`.

plan 6;

# Following the recursion measured `<A>` as 4 here and picked it; Rakudo cuts
# the inner `<A>` into a fate, so `<A>` measures 1 and `'aab'` (3) wins.
grammar H {
    token A { 'a' <A>? 'b' | 'q' }
    token T { [ <A> | 'aab' ] }
}
is H.subparse("aabb", :rule<T>).Str, 'aab', 'a recursive call inside the ranked rule ends its path';

# The ranked branch's own rule is still inlined once, even when the
# alternation sits inside that same rule.
grammar G {
    token TOP { <A> }
    token A { 'x' [ <A> | 'x' ] }
}
is G.subparse("xxx").Str, 'xxx', 'the enclosing rule is inlined once';
is G.subparse("xxxx").Str, 'xxxx', 'ranking falls back to declaration order on a tie';

# A proto's dispatch inlines a candidate's call to the proto itself once.
grammar P {
    token TOP { <t> }
    proto token t {*}
    token t:sym<a> { 'x' <t> }
    token t:sym<b> { 'xx' }
}
is P.subparse("xxxx").Str, 'xxxx', 'proto dispatch inlines the proto once';

# Mutual recursion is cut the same way.
grammar R {
    token TOP { <u> }
    token u { <t> }
    token t { 'x' <u> | 'xx' }
}
is R.subparse("xxxx").Str, 'xxxx', 'mutual recursion is cut at the second entry';

# The #9617 shape (as a `token`): `<A>` is ranked at every position.
grammar A1 {
    token TOP { <A> }
    token A { '{' [ <A> | . ]*? '}' }
}
is A1.parse('{' ~ ('{ab{c}d}' x 16) ~ '}').<A>.to, 130, 'a recursive frugal token parses';
