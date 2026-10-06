use Test;

plan 6;

# #12130: an `our sub` that rebinds a file-scope `my` with `:=` must change the
# variable, both for its own later reads and for the declaring scope.
my $buf := [1, 2];
our sub rebind() { $buf := []; $buf.elems }
is rebind(), 0, 'our sub sees its own `:=` rebind of a file-scope scalar';
is $buf.elems, 0, 'the declaring scope sees the rebind';

my $other := [1, 2, 3];
my $alias := $other;
our sub rebind-other() { $other := [9]; $other[0] }
is rebind-other(), 9, 'rebind to a fresh array is visible in the sub';
is $other.elems, 1, 'declaring scope sees the new binding';
is $alias.elems, 3, 'a name bound to the old container keeps it';

my $n = 5;
our sub assign() { $n = 7; $n }
is assign(), 7, 'plain assignment from an our sub is unaffected';
