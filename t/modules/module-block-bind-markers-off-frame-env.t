use v6;
use Test;

plan 7;

# A `:=` binding records its shape in companion markers keyed by the bare
# name (`__mutsu_bound_decont::`, `__mutsu_scalar_bind_no_container::`). Those
# keys contain `::`, so a `module X { my $a := … }` block's exit carried them
# out with the package-qualified names, although the binding itself is
# dropped there; `use JSON::Fast` left 20 of them in every frame env
# (ADR-0084, #7817). They now end with the block. What is pinned here is that
# every binding they describe still behaves as before.

# An outer binding made before the block keeps its markers.
my $outer := (1, 2, 3);

module BindMarkers {
    my $list := (1, 2, 3);
    my $num := 42;
    our sub num() { $num }
    our sub assign-num() { try { $num = 1; 'assigned' } // 'died' }
    our sub list-elems() { $list.elems }
}

my $n = 0;
for $outer { $n++ }
is $n, 3, 'an outer list binding made before the block still iterates as a list';
throws-like { $outer = 5 }, Exception, '... and still has no container';

is BindMarkers::num(), 42, "the block's routine reads its value binding";
is BindMarkers::assign-num(), 'died', '... which still has no container';
is BindMarkers::list-elems(), 3, "the block's routine reads its list binding";

# The block's names are free for the mainline to declare afresh.
my $list = [7, 8];
my $m = 0;
for $list { $m++ }
is $m, 1, 'a mainline itemized array of the same name iterates as one item';
my $num = 5;
$num = 6;
is $num, 6, 'a mainline variable of the same name assigns';
