use v6;
use Test;

# A numbered capture ($0, $1, ...) is a Match, so a subscript in string
# interpolation accesses its nested captures. Regression: mutsu left the
# `[0]` as a literal fragment, printing "abc[0]" instead of the sub-capture.
# (raku-doc Language/regexes.rakudoc)

plan 6;

if 'abc' ~~ / ( a (.) (.) ) / {
    is "$0", "abc",               'plain $0 still interpolates the whole capture';
    is "$0[0]", "b",              '$0[0] interpolates the first nested capture';
    is "$0[1]", "c",              '$0[1] interpolates the second nested capture';
    is "Inner: $0[0] and $0[1]", "Inner: b and c",
                                  'both nested subscripts interpolate in one string';
}

if 'abc' ~~ / ( a $<x>=(.) . ) / {
    is "$0<x>", "b",              '$0<name> interpolates a named sub-capture';
}

if 'abc' ~~ / (.) (.) (.) / {
    is "$0.uc()", "A",            'method call on a numbered capture still works';
}
