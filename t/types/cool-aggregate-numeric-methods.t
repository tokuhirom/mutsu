use Test;

plan 9;

# List/Array and Map/Hash are Cool: Cool's numeric methods numify them to
# their element count.
is {a => 1, b => 2}.round, 2, 'Hash.round';
is [1, 2, 3].floor, 3, 'Array.floor';
is (1, 2).ceiling, 2, 'List.ceiling';
is {a => 1}.abs, 1, 'Hash.abs';
is [1, 2, 3].sign, 1, 'Array.sign';
is-approx {a => 1, b => 2}.sqrt, 2.sqrt, 'Hash.sqrt';
is {a => 1}.round(0.5), 1, 'Hash.round($scale)';
is [1, 2].log(2), 1, 'Array.log($base)';

# nodemap does not descend into a nested Hash: each one is rounded as a number.
my %h = a => {x => 1.26, y => 2.71}, b => {z => 3.3};
is-deeply %h.nodemap({ $_.round(0.1) }), {a => 2.0, b => 1.0}, 'nodemap rounds nested hashes as numbers';
