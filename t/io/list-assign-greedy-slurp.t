use Test;

# In Raku positional list assignment, the FIRST @/% target is greedy: it slurps
# all remaining RHS values, and every target after it gets an empty container /
# Nil. This holds for both `my (...)` destructuring declarations and runtime
# assignment to an existing parenthesized target list.

plan 13;

# --- my-declaration destructuring ---
{
    my ($x, @b, $c) = 1..4;
    is $x, 1,            'decl: leading scalar takes one element';
    is ~@b, '2 3 4',     'decl: middle array slurps the rest';
    nok $c.defined,      'decl: scalar after greedy array is undefined';
}
{
    my (@a, $b) = 1, 2, 3;
    is ~@a, '1 2 3',     'decl: leading array slurps everything';
    nok $b.defined,      'decl: trailing scalar after array is undefined';
}
{
    my ($a, %h) = "!", a => 1, b => 2, c => 3;
    is $a, "!",                         'decl: scalar before hash';
    is %h.keys.sort.join(","), "a,b,c", 'decl: hash slurps the remaining pairs';
}
{
    my ($a, @b, %h) = 1, 2, 3, (x => 4);
    is @b.elems, 3,   'decl: first array is greedy (slurps the trailing pair too)';
    is %h.elems, 0,   'decl: hash after greedy array is empty';
}

# --- runtime assignment to existing targets ---
{
    my (@b, $c);
    (*, @b, $c) = 1..4;
    is ~@b, '2 3 4',   'runtime: * skip then greedy array slurp';
    nok $c.defined,    'runtime: scalar after greedy array is undefined';
}
{
    my ($x, %h);
    ($x, %h) = "!", a => 1, b => 2;
    is $x, "!",                       'runtime: scalar before hash';
    is %h.keys.sort.join(","), "a,b", 'runtime: hash slurps remaining pairs';
}
