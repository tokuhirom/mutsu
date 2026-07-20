use Test;

# A numbered match variable is subscriptable inside string interpolation:
# `"$0[0]"` is `$/[0][0]` (first positional subcapture), `$0<k>` a named one.
# mutsu used to interpolate `$0` (the whole Match) and leave `[0]` literal.
# (Language/regexes.rakudoc, doc-diff finding [17].)

plan 6;

if 'abc' ~~ / ( a (.) (.) ) / {
    is "$0[0]",     'b',       '$0[0] indexes first positional subcapture';
    is "$0[1]",     'c',       '$0[1] indexes second positional subcapture';
    is "$0",        'abc',     'bare $0 still interpolates the whole capture';
    is "$0[0].uc()", 'B',      '$0[0].method() chains after the subscript';
}

if 'abc' ~~ / ( a $<mid>=(.) . ) / {
    is "$0<mid>",   'b',       '$0<key> indexes a named subcapture';
}

if 'xy' ~~ / (.) (.) / {
    is "$0 and $1", 'x and y', 'top-level $0/$1 unaffected';
}
