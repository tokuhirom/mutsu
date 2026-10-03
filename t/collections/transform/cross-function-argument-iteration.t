use Test;

plan 6;

# cross() iterates each argument as-is; only a single argument is taken as
# the list of lists.
is-deeply cross([(0, 1),], [(1, 2), (2, 3)]).List, (((0, 1), (1, 2)), ((0, 1), (2, 3))),
    'a one-element list holding a list contributes that list';
is-deeply cross([[1, 2],], (3,)).List, (([1, 2], 3),), 'same with an Array element';
is-deeply cross([1, 2], (3, 4)).List, ((1, 3), (1, 4), (2, 3), (2, 4)), 'plain lists';
my $x = (1, 2);
is-deeply cross($x, (3, 4)).List, (((1, 2), 3), ((1, 2), 4)), 'an itemized list is one element';
my %h = %(:a);
is-deeply cross(%h<>:v.map: *.flat), ((True,),), 'single-argument rule';
my @range-pairs = [0, 1].rotor(2 => -1), [1, 2, 3, 5].rotor(2 => -1);
is cross(|@range-pairs>>.Array).elems, 3, 'arrays of rotor pairs cross pairwise';
