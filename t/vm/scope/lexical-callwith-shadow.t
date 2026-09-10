use v6;
use Test;

plan 4;

# A lexically-bound `&callwith` / `&callsame` / `&nextwith` / `&nextsame` /
# `&samewith` shadows the built-in dispatcher routine of the same name
# (roast/integration/advent2013-day21.t "pointy block syntax").
{
    my &callwith := -> *@pos, *%named { @pos => %named };
    is-deeply callwith(10, 20, :a(30), :b(40)),
        [10, 20] => {a => 30, b => 40},
        'lexical &callwith shadows the dispatcher builtin';
}
{
    my &callsame := -> $x { $x * 2 };
    is callsame(21), 42, 'lexical &callsame shadows the dispatcher builtin';
}
{
    my &samewith := -> $x, $y { $x ~ $y };
    is samewith('a', 'b'), 'ab', 'lexical &samewith shadows the dispatcher builtin';
}
# The builtin still works when NOT shadowed (real dispatcher semantics).
{
    my $log = '';
    multi fmt(Int $n) { $log ~= "int "; callsame }
    multi fmt(Any $x) { $log ~= "any "; "[$x]" }
    fmt(5);
    is $log, 'int any ', 'unshadowed callsame still dispatches to the next candidate';
}
