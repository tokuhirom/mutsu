use Test;

# A parse failure is a *syntax* error, so it carries an X::Syntax:: class that
# `throws-like` and a typed `CATCH { when ... }` can dispatch on. Raku's
# catch-all for a construct it cannot describe more precisely is
# X::Syntax::Confused; specific shapes get the class Raku names for them.

plan 12;

# The catch-all.
throws-like q:to/CODE/, X::Syntax::Confused,
    my @b = 1, 2;
    say 1 if @b.elems
    say 2
    CODE
    'two terms in a row is X::Syntax::Confused';

throws-like 'say 1 ]', X::Syntax::Confused,
    'an unexpected closing bracket is X::Syntax::Confused';

# A bareword type name gets the same null-component check the sigilled forms
# (`$a::::b`) already had.
throws-like 'Foo::::Bar.new', X::Syntax::Name::Null,
    'a bareword type name may not have a null component';

# A `-->` return constraint is only legal as the last element of a signature.
throws-like 'sub foo (--> Bool, Int $y) { True }', X::Syntax::Malformed,
    'a return constraint before a parameter is malformed';

throws-like 'sub foo ($x, --> Bool, Int $y) { True }', X::Syntax::Malformed,
    'a return constraint in the middle of the parameters is malformed';

throws-like 'sub foo ($x; --> Bool; Int $y) { True }', X::Syntax::Malformed,
    'the same across a multidimensional `;` signature';

# The legal shapes still parse.
lives-ok { EVAL 'sub ok1 ($x, $y --> Bool) { True }' },
    'a trailing return constraint still works';
lives-ok { EVAL 'sub ok2 (--> Bool) { True }' },
    'a signature that is only a return constraint still works';
lives-ok { EVAL 'sub ok3 (@a; @b) { @a.elems + @b.elems }' },
    'a multidimensional `;` signature still works';

# A variable name may not open with a digit -- of any script -- nor with a
# combining mark.
throws-like 'my $10kinds', X::Syntax::Variable::Numeric,
    'an ASCII-digit-initial variable cannot be declared';

throws-like qq{my \$\c[BENGALI DIGIT ONE]\c[BENGALI DIGIT ZERO]kinds},
    X::Syntax::Variable::Numeric,
    'a non-ASCII-digit-initial variable cannot be declared either';

throws-like qq{my \$\c[COMBINING DIAERESIS]a;}, X::Syntax::Malformed,
    'a combining mark may not open an identifier';
