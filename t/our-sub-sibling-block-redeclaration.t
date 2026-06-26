use Test;

# `our sub` is package-scoped: two `our sub foo` declarations install the same
# package symbol, so a duplicate across sibling blocks (or block vs mainline) is
# X::Redeclaration. `my sub` is lexical and does NOT conflict across blocks.

plan 4;

throws-like '{ our sub foo { say "OMG" } }; { our sub foo { say "WTF" } };',
    X::Redeclaration, 'our sub in sibling blocks is X::Redeclaration';

# `my sub` in separate blocks is lexical — no conflict.
lives-ok { EVAL '{ my sub foo { 1 } }; { my sub foo { 2 } }' },
    'my sub in sibling blocks lives (lexical)';

# A single our sub is fine.
lives-ok { EVAL '{ our sub foo { 42 } }' }, 'single our sub lives';

# Different names do not conflict.
lives-ok { EVAL '{ our sub foo { 1 } }; { our sub bar { 2 } }' },
    'different our sub names live';
