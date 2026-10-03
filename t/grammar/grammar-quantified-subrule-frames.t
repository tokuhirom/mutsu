# ADR-0135 Slice E: a quantified subrule call (`<x>*`, `<x>+`, `<x> ** n`,
# `<x>+ % sep`) is a loop of frame calls on the compiled regex engine. Under
# ratchet each iteration commits to its callee's first end; in a `regex`, a
# later failure backtracks into an iteration's callee. Values are rakudo's.
use Test;

plan 9;

# Backtracking into a quantified call's callee (`x` gives back one `a`).
{
    grammar Back {
        regex TOP { <x>+ a }
        regex x { a+ }
    }
    my $m = Back.parse('aaa');
    ok $m, 'a non-ratchet quantified call backtracks into its callee';
    is $m<x>.map(~*).join('|'), 'aa', 'the iteration gave back one character';
}

grammar Sep {
    token TOP { <w>* % ',' }
    token w { \w+ }
}
is Sep.parse('ab,cd')<w>.map(~*).join('|'), 'ab|cd', 'a separated quantified call';

grammar Counted {
    token TOP { <d> ** 2..3 }
    token d { \d }
}
is Counted.parse('123')<d>.elems, 3, 'a counted quantified call';
nok Counted.parse('1234'), 'the count bounds the iterations';

grammar Group {
    token TOP { [<d> <.sep>?]+ }
    token d { \d }
    token sep { ',' }
}
is Group.parse('1,2,3')<d>.elems, 3, 'calls inside a quantified group';

# A ratcheted loop is possessive: the iterations are not given back.
grammar Possessive {
    token TOP { <a>+ a }
    token a { a }
}
nok Possessive.parse('aaa'), 'a ratcheted quantified call does not give back iterations';

# A quantified proto call dispatches each iteration's action by its candidate.
grammar Proto {
    token TOP { <item>+ }
    proto token item {*}
    token item:sym<a> { a }
    token item:sym<b> { b }
}
class ProtoActions {
    method item:sym<a>($/) { make 'A' }
    method item:sym<b>($/) { make 'B' }
    method TOP($/) { make $<item>».made.join }
}
is Proto.parse('aba', :actions(ProtoActions)).made, 'ABA', 'each iteration runs its candidate action';
is Proto.parse('abba')<item>.elems, 4, 'every iteration is filed';
