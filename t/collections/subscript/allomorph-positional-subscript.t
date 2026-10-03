use Test;
# From Terminal::UI: `Frame.new(:top<1>)` stores an IntStr that is later used as
# an array row index (`@.chars[$r] //= []`).
plan 7;
my @a = 5, 6, 7;
my $r = <1>;
is @a[$r], 6, 'IntStr scalar as positional index';
is @a[<1>], 6, 'IntStr literal as positional index';
is-deeply @a[<1 2>].List, (6, 7), 'allomorph word-list slice';
my @b; @b[1] = [5, 6];
@b[$r] //= [9];
is-deeply @b[1], [5, 6], '//= on an existing slot indexed by IntStr keeps the value';
my %h = 1 => 'x';
is %h{$r}, 'x', 'hash subscript unaffected';
is-deeply (Any[1..3] ~~ Any), True, 'sanity';
my @c; @c[2] = [1, 2];
is-deeply @c[1][1..3].List, (Any, Any, Any), 'range slice of a missing row is a list of Any';
