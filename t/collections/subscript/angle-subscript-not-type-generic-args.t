use Test;

plan 10;

# #8295: `Type<...>` after a bare type-object term is the ordinary
# postcircumfix word-list subscript, never `Type[...]` parameterization sugar
# -- rakudo has no such syntax. Verified against rakudo.

is Array<Int>.raku, Any.raku, 'Array<Int> is Any, not Array[Int] parameterization';

is-deeply Hash<A, B, C>.List, (Any, Any, Any),
    'Hash<A, B, C> is a three-key slice of Any, not Hash[A,B,C]';

is Array[Int].^name, 'Array[Int]', 'Array[Int] (bracket) still genuinely parameterizes';

{
    class NonParametricAngle { }
    is NonParametricAngle<Int>.raku, Any.raku,
        'a non-parametric class subscripted with <...> still answers Any, not X::NotParametric';
}

{
    dies-ok { EVAL 'class NonParametricBracket { }; NonParametricBracket[Int]' },
        'the same shape of class parameterized with [...] (bracket) still dies X::NotParametric';
}

{
    role BracketRole[::T] { }
    is BracketRole[Int].^name, 'BracketRole[Int]',
        'a role parameterized with [...] (bracket) is unaffected';
}

{
    my $h;
    is-deeply $h<a b c>.List, (Any, Any, Any),
        'a bare Any value subscripted with <...> is unaffected (the pre-existing case this generalizes from)';
}

{
    # A `[...]` parameterization of a bare type object inside a list literal
    # or a `:=` bind routes through the autovivify-lazy-terminal opcode rather
    # than the plain Index op, whose Package fallback used to hardcode
    # `is_positional: false` regardless of the real subscript. That was
    # harmless before this ticket (every Package subscript parameterized
    # identically either way) but turned wrong once the `<...>`-vs-`[...]`
    # distinction above started to matter.
    my @t = (array[int],);
    is @t[0].^name, 'array[int]',
        'array[int] (bracket) inside a list literal still genuinely parameterizes, not misread as <...>';
}

{
    # A bare package/type object subscripted with <...> directly is the same
    # Any-returning case as the class/role checks above -- navigation only
    # happens through a genuine Associative (e.g. .WHO, which answers a real
    # Stash), never through the type object itself.
    class Ns8295Direct { }
    is GLOBAL<Ns8295Direct>.raku, Any.raku,
        'GLOBAL<Name> (direct, no .WHO) is Any, not stash navigation';
    is GLOBAL.WHO<Ns8295Direct>.^name, 'Ns8295Direct',
        'GLOBAL.WHO<Name> (through the real Associative stash) does navigate';
}
