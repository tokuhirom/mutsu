use Test;

# A bareword that names an in-scope term (sigilless binding or constant)
# followed by ` | ` is the infix junction operator, not a listop call
# with a slipped argument (issue #9364).

plan 8;

{
    my &f = -> \g, \e { g | e };
    is f(1, 2).raku, 'any(1, 2)', 'pointy-block sigilless params: g | e';
}

{
    sub f(\g, \h) { g | h }
    is f(1, 2).raku, 'any(1, 2)', 'sub sigilless params: g | h';
}

{
    my \x = 1;
    my \y = 2;
    is (x | y).raku, 'any(1, 2)', 'my-declared sigilless terms';
}

{
    my \a = 1;
    sub inner { my \b = 2; a | b }
    is inner().raku, 'any(1, 2)', 'enclosing-scope sigilless term on the left';
}

{
    constant c = 4;
    constant d = 1;
    is (c | d).raku, 'any(4, 1)', 'constants on both sides';
}

{
    my &id = -> \g, \e { g | e ~~ Seq:D ?? 'seq' !! 'other' };
    is id(1, 2), 'other', 'the Iter::Able helper shape parses as an infix';
}

{
    my \c = \(1, 2);
    sub foo(*@a) { @a.elems }
    is (foo |c), 2, 'a declared listop still slips its capture';
}

{
    my \c = \(1, 2, 3);
    is (bar |c), 3, 'a post-declared listop still slips its capture';
    sub bar(*@a) { @a.elems }
}
