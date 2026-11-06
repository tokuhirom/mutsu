use Test;

plan 4;

# `BIND-POS` takes its value argument raw: binding `@array[i]` keeps the
# caller's element container, so a write through the bound slot reaches it.
my @array = 5, 3, 8;
my @descent;
@descent.BIND-POS(0, @array[2]);
my $node := @descent[0];
$node = 100;
is-deeply @array, [5, 3, 100], 'a write through the bound slot reaches the source element';

my @other = 1, 2, 3;
my @bound;
@bound.BIND-POS(1, @other[0]);
@bound[1] = 42;
is-deeply @other, [42, 2, 3], 'direct assignment to the bound slot too';

my %h = a => 1;
my %bound;
%bound.BIND-KEY('k', %h<a>);
%bound<k> = 7;
is %h<a>, 7, 'BIND-KEY keeps the hash element container';

my @plain;
@plain.BIND-POS(0, 5);
is @plain[0], 5, 'a plain value still binds';
