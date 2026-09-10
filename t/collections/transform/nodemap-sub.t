use v6;
use Test;

# `nodemap` has a sub form (`nodemap &op, \obj`) as well as the method form.
# It applies its callable to each top-level element without descending into
# sublists, and on an Associative it acts on the values. Previously the sub
# form failed to parse ("Two terms in a row"/numify error) and the Hash case
# of the method form mapped the whole hash as one value.

plan 8;

# Sub form over nested lists (no recursion into sublists).
is (nodemap *+1, [[1,2,3], [[4,5],6,7], 7]).raku, '(4, 4, 8)',
    'nodemap sub over nested lists counts sublist elems + 1';
is (nodemap *+1, (10, 20, 30)).raku, '(11, 21, 31)', 'nodemap sub over a flat list';

# Hyper method form.
is ([[2, 3], [4, [5, 6]]]».nodemap(*+1)).raku, '((3, 4), (5, 3))',
    'hyper nodemap over a list of lists';

# On an Associative, nodemap acts on the values and returns a Hash.
is (nodemap *.flip, { what => "is", this => "thing" }).raku,
    '{:this("gniht"), :what("si")}', 'nodemap sub on a hash maps the values';
my %h = a => "xy", b => "pq";
is %h.nodemap(*.flip).raku, '{:a("yx"), :b("qp")}', 'nodemap method on a hash maps the values';

# nodemap does not flatten Slips (unlike map).
is [[2,3], [[4,5],6,7], 7].nodemap({.elems == 1 ?? $_ !! slip}).gist, '(() () 7)',
    'nodemap keeps empty Slips as empty lists';

# Method form returns a List even from an Array.
is [2, 3].nodemap(*+1).^name, 'List', 'nodemap returns a List';
is <a b c>.nodemap(*.uc).raku, '("A", "B", "C")', 'nodemap over a word list';
