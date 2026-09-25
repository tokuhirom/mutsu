use Test;

# #9173: a class's C3 linearization is memoized when the class is declared,
# and the merge runs in O(names * parents). A chain of d classes used to cost
# O(d^3) to declare (every declaration re-walked every ancestor's MRO).

plan 5;

use MONKEY-SEE-NO-EVAL;

{
    my $src = 'class LC0 { method m { "base" } }; '
        ~ (1..300).map({ "class LC$_ is LC{$_ - 1} \{ \}; " }).join
        ~ 'LC300';
    my $top = EVAL $src;
    is $top.new.m, 'base', 'a method resolves through a 300-class chain';
    is $top.^mro.elems, 303, 'the chain MRO lists every class plus Any and Mu';
    is $top.^mro[0, 1, *-3, *-2, *-1].map(*.^name).join(' '), 'LC300 LC299 LC0 Any Mu',
        'the chain MRO is in order';
}

{
    class DA { }
    class DB is DA { }
    class DC is DA { }
    class DD is DB is DC { }
    is DD.^mro.map(*.^name).join(' '), 'DD DB DC DA Any Mu', 'C3 order of a diamond';
}

throws-like 'class Y1 { }; class Y2 is Y1 { }; class Y3 is Y1 is Y2 { }', Exception,
    message => /:i 'inconsistent' | 'C3'/, 'an inconsistent hierarchy is rejected';
