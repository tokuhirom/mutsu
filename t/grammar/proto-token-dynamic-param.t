use Test;

plan 3;

# A proto token's own `$*` parameter is bound for its candidates (#11071).
grammar Proto2 {
    token TOP { <p('k')> }
    proto token p($*K) {*}
    token p:sym<a> { a <?{ $*K eq 'nope' }> }
    token p:sym<b> { a <?{ $*K eq 'k' }> }
}
ok Proto2.parse('a'), 'proto $*K reaches the candidates';

grammar Proto3 {
    token TOP { <p('z')> }
    proto token p($*K) {*}
    token p:sym<a> { a <?{ $*K eq 'k' }> }
}
nok Proto3.parse('a'), 'a non-matching binding still fails';

grammar Proto4 {
    token TOP { <p> }
    proto token p($*K = 'dflt') {*}
    token p:sym<a> { a <?{ $*K eq 'dflt' }> }
}
ok Proto4.parse('a'), 'proto default for $*K is used';

done-testing;
