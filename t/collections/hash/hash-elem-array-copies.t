use Test;

plan 5;

my %h = a => 1;
my @p = %h, %h;
ok %h ∈ @p, 'a Hash is an element of the Array built from it';
ok @p[0] ∈ @p, 'an element of the Array is an element of it';
ok %h (elem) @p, '(elem) spelling';
ok %h === @p[0], '=== agrees';
my %other = a => 1;
nok %other ∈ @p, 'an equal but distinct Hash is not an element';
