use Test;

# A core operator's routine value (`&[cmp]`) is that operator: a lexical
# `&infix:<cmp>` in the scope that later CALLS the value is not consulted.

plan 7;

sub call(&c, $a, $b) { c($a, $b) }
my &plain = &[cmp];

is call(&plain, 1, 2), Less, 'the value called through a parameter';
{
    my &infix:<cmp> = -> $a, $b { "lexical" };
    is call(&plain, 1, 2), Less, 'a lexical override in the calling scope is not used';
    is plain(1, 2), Less, 'nor by a direct call of the value';
    my &c = &[cmp];
    is c(1, 2), "lexical", '&[cmp] taken inside the override is the override';
    is 1 cmp 2, "lexical", 'the operator form sees the override';
}

sub f(:&cmp = &[cmp]) { cmp(2, 1) }
is f(), More, 'a :&cmp default';
is (&[eq])("a", "a"), True, 'another plain operator value';
