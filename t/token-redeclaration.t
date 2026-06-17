use Test;

plan 5;

# Two `my`/`our`-scoped tokens of the same name in one grammar body are an
# X::Redeclaration (a plain `token foo` redeclared is X::Method::Duplicate).
throws-like 'my grammar G { my token foo { x }; my token foo { y } }',
    X::Redeclaration, 'duplicate my token';
throws-like 'my grammar G { our token foo { x }; our token foo { y } }',
    X::Redeclaration, 'duplicate our token';

# Distinct token names are fine, and a working grammar still parses.
{
    lives-ok { EVAL 'my grammar G { my token a { x }; my token b { y } }' },
        'distinct my tokens are allowed';
}
{
    grammar G { token TOP { \d+ } }
    ok G.parse('42'), 'an ordinary grammar still parses';
}
{
    grammar H { token a { 'x' }; token b { 'y' }; token TOP { <a><b> } }
    ok H.parse('xy'), 'distinct plain tokens still compose';
}
