use Test;

plan 5;

# `<value:sym<number>>` calls one candidate of a proto, not the whole proto.
grammar N {
    proto token value {*}
    token value:sym<number> { '-'? \d+ }
    token value:sym<word> { <[a..z]>+ }
    token one { <value:sym<number>> }
    token alt { <value:sym<number>> | <id> }
    token id { <[a..z]>+ }
}
is N.subparse('12', :rule<one>).to, 2, 'the named candidate matches';
nok N.subparse('ab', :rule<one>), 'the other candidates are not tried';
ok N.subparse('12', :rule<one>){'value:sym<number>'}, 'captured under the long name';
is N.subparse('7', :rule<alt>).to, 1, 'inside an alternation (first branch)';
is N.subparse('ab', :rule<alt>).to, 2, 'the next branch is still reached';
