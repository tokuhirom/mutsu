use Test;

plan 4;

# A subrule call's argument expressions run in the caller's scope, so an
# assignment inside one lands in the caller's lexical (#10612).
{
    my $c = 0;
    grammar G1 { token TOP { <x($c++)> }; token x($n) { a } }
    G1.parse("a");
    is $c, 1, 'postfix ++ in a subrule argument updates the caller lexical';
}

{
    my $calls = 0;
    grammar G2 { token TOP { <x({ $calls++; 2 }())> }; token x($n) { a } }
    G2.parse("a");
    is $calls, 1, 'a block call in a subrule argument updates the caller lexical';
}

{
    my $seen = -1;
    grammar G3 { token TOP { <x($seen = 7)> }; token x($n) { a { $seen = $n } } }
    G3.parse("a");
    is $seen, 7, 'an assignment argument binds and updates the lexical';
}

{
    my $c = 0;
    my regex yy($n) { a }
    "aa" ~~ / <yy($c++)> <yy($c++)> /;
    is $c, 2, 'each subrule call evaluates its arguments once, in a plain regex';
}
