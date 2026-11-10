use Test;

plan 4;

my %i = (a => 1, r => 'old');
is-deeply {%i, r => 'new'}, {a => 1, r => 'new'}, 'a later pair overrides the spread hash';
is-deeply {r => 'first', %i}, {a => 1, r => 'old'}, 'a later spread overrides an earlier pair';
is-deeply {a => 0, %i, a => 2}, {a => 2, r => 'old'}, 'pairs on both sides of the spread';
is-deeply {a => 1, b => 2}, {a => 1, b => 2}, 'a plain composer is unchanged';

# vim: expandtab shiftwidth=4
