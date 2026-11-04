use v6;
use Test;

plan 12;

# A plain term the bareword classifier resolves (a sigilless declaration, a
# constant, a setting term) is lowered by the one term function, without the
# node round trip (ADR-15760 slice 2).

my \s = 5;
is s, 5, 'a sigilless term named like a quote operator';
constant zz = 3;
is zz + 1, 4, 'a lowercase constant';
my \target = 'a';
is target, 'a', 'a sigilless term';
sub f(\c) { c }
is f(7), 7, 'a sigilless parameter';
is-approx pi, 3.14159265358979, 'pi';
is-approx tau, 6.28318530717959, 'tau';
ok True, 'True';
is Empty.elems, 0, 'Empty';
# A user term shadows the math constant.
{
    my \e = 'mine';
    is e, 'mine', 'a sigilless e shadows the constant';
}
# A label is not a term.
my $n = 0;
LOOP1: for 1..3 { for 1..3 { $n++; next LOOP1 } }
is $n, 3, 'a loop label stays a label';
# A capitalised word stays the word for the compiler to resolve, so a local
# declaration of a keyword-like name wins.
{
    constant True = 42;
    is True, 42, 'a local constant named True';
}
{
    my \False = 'no';
    is False, 'no', 'a sigilless term named False';
}
