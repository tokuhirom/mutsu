use Test;

# #9643: a proto called inside a declarative prefix is measured as the union
# of every candidate's every end, as Rakudo's NFA inlines it, not as the
# ranked winner's greedy end alone. In each case the union reaches past the
# literal branch, so the `<t>` branch ranks first and then matches for real
# through its shorter tail. Expected values checked against `raku`.

plan 6;

# Through the NFA (a `|` branch ranked from a real match).
grammar P1 {
    proto token t {*}
    token t:sym<a>  { 'a' }
    token t:sym<ab> { 'ab' }
    token TOP { [ <t> [ 'bcde' | 'c' ] { make 'A' } | 'abcd' { make 'B' } ] .* }
}
is P1.parse("abcde").made, 'A', 'a losing candidate still extends the prefix';

grammar P2 {
    proto token t {*}
    token t:sym<x> { 'a' 'b'? }
    token TOP { [ <t> [ 'bcde' | 'c' ] { make 'A' } | 'abcd' { make 'B' } ] .* }
}
is P2.parse("abcde").made, 'A', "a candidate's shorter end extends the prefix";

grammar P3 {
    proto token t {*}
    token t:sym<foo> { <sym> }
    token t:sym<fo>  { <sym> }
    token TOP { [ <t> [ 'obarx' | 'b' ] { make 'A' } | 'foob' { make 'B' } ] .* }
}
is P3.parse("foobarx").made, 'A', '<sym> matches each candidate its own sym';

grammar P4 {
    proto token t {*}
    token t:sym<a>  { 'a' }
    token t:sym<ab> { 'ab' }
    token TOP { [ <t> 'xyz' { make 'A' } | 'a' { make 'B' } ] .* }
}
is P4.parse("abxyz").made, 'A', 'the winning candidate is still measured';

# Through the walker: the candidates of an outer proto are ranked by the
# walker's measurement (ADR-0046), which sees the inner proto the same way.
# (`regex`, not `token`: the walker still honours `:ratchet` while measuring,
# which Rakudo's NFA does not; see ADR-0125 §4.)
grammar W1 {
    proto token t {*}
    token t:sym<a>  { 'a' }
    token t:sym<ab> { 'ab' }
    proto token u {*}
    regex u:sym<one> { <t> [ 'bcde' | 'c' ] }
    token u:sym<two> { 'abcd' }
    token TOP { <u> .* }
}
is W1.parse("abcde")<u>.Str, 'abc', 'an outer proto ranks by the inner union';

grammar W2 {
    proto token t {*}
    regex t:sym<x> { 'a' 'b'? }
    proto token u {*}
    regex u:sym<one> { <t> [ 'bcde' | 'c' ] }
    token u:sym<two> { 'abcd' }
    token TOP { <u> .* }
}
is W2.parse("abcde")<u>.Str, 'abc', "an outer proto sees a candidate's shorter end";
