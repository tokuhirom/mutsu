use Test;

plan 4;

my @floors = ('A', ('B', 'C', ('E', 'F', 'G')));
is @floors.tree(1).flat.elems, 6,
    '.tree(1) exposes nested Array elements without an extra item level';
is @floors.tree(1).raku, '$(("A", ("B", "C", ("E", "F", "G"))).Seq)',
    '.tree(1) preserves only the outer tree itemization';

my $nested = [[1, 2, 3], [(4, 5), 6, 7]];
is $nested[1].List.raku, '((4, 5), 6, 7)',
    '.List decontainerizes elements of an itemized Array';
is-deeply $nested».List.flat.List, (1, 2, 3, 4, 5, 6, 7),
    'hyper .List followed by .flat has no extra nested item level';
