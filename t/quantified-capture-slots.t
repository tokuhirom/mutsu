use Test;

plan 21;

# Index-stable positional capture slots: an unmatched optional capture reserves
# its slot so following captures keep their number. A `(x)?` matching zero times
# yields Nil; a `(x)*` / `(x)+` (zero or more) always yields a List (empty when
# it matched zero times). Verified against Rakudo.

# (a)?(b) on "b": $0 = Nil, $1 = Match(b)
{
    'b' ~~ /(a)?(b)/;
    is $/.list.elems, 2, '(a)?(b): two positional slots';
    nok $/[0].defined,   '(a)?(b): $0 is Nil (unmatched optional)';
    is ~$/[1], 'b',      '(a)?(b): $1 is b';
}

# (a)?(b) on "ab": both match
{
    'ab' ~~ /(a)?(b)/;
    is ~$/[0], 'a', '(a)?(b) on ab: $0 = a';
    is ~$/[1], 'b', '(a)?(b) on ab: $1 = b';
}

# (z)* matching zero times yields an empty list
{
    'x' ~~ /(z)*/;
    is $/.list.elems, 1,            '(z)* on x: one slot';
    ok $/[0] ~~ Positional,         '(z)* on x: $0 is Positional';
    is $/[0].elems, 0,              '(z)* on x: $0 is empty list';
}

# (z)* matching twice yields a 2-element list
{
    'zz' ~~ /(z)*/;
    ok $/[0] ~~ Positional, '(z)* on zz: $0 is Positional';
    is $/[0].elems, 2,      '(z)* on zz: two elements';
}

# (y)?(z)* on "x": $0 = Nil, $1 = empty list
{
    'x' ~~ /(y)?(z)*/;
    is $/.list.elems, 2,    '(y)?(z)* on x: two slots';
    nok $/[0].defined,      '(y)?(z)* on x: $0 Nil';
    ok $/[1] ~~ Positional && $/[1].elems == 0, '(y)?(z)* on x: $1 empty list';
}

# (a)?(b)?(c) on "c": $0 = Nil, $1 = Nil, $2 = c
{
    'c' ~~ /(a)?(b)?(c)/;
    is $/.list.elems, 3, '(a)?(b)?(c) on c: three slots';
    nok $/[0].defined,   '(a)?(b)?(c) on c: $0 Nil';
    nok $/[1].defined,   '(a)?(b)?(c) on c: $1 Nil';
    is ~$/[2], 'c',      '(a)?(b)?(c) on c: $2 = c';
}

# (z)*(a)? on "a": $0 = empty list, $1 = a
{
    'a' ~~ /(z)*(a)?/;
    ok $/[0] ~~ Positional && $/[0].elems == 0, '(z)*(a)? on a: $0 empty list';
    is ~$/[1], 'a',     '(z)*(a)? on a: $1 = a';
}

# Alternation does NOT reserve across branches (Rakudo: elems == 1).
{
    'b' ~~ /(a)|(b)/;
    is $/.list.elems, 1, '(a)|(b) on b: one slot (branch-reset numbering)';
    is ~$/[0], 'b',      '(a)|(b) on b: $0 = b';
}
