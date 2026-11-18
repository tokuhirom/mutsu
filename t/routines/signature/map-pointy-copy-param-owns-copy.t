use Test;

# An `is copy` parameter of a block called by `.map` owns a copy of the
# element: changing it leaves the source alone (#18151, CSS::Specification::
# Compiler's `@modules.map: -> @module is copy { @module.append: $type; ... }`).

plan 6;

my @m = [1, 2], [3];
my @sizes = @m.map(-> @x is copy { @x.push: 9; @x.elems });
is-deeply @sizes, [3, 2], 'the block sees its own pushes';
is-deeply @m, [[1, 2], [3]], 'the source arrays are untouched';

my @again = @m.map(-> @x is copy { @x.append: 7; @x.elems });
is-deeply @again, [3, 2], 'a second map starts from the same elements';

my $a = [1, 2];
($a,).map(-> @x is copy { @x.push: 9; 1 }).eager;
is-deeply $a, [1, 2], 'a List source too';

my @n = 1, 2, 3;
@n.map(-> $_ is copy { $_++ }).eager;
is-deeply @n, [1, 2, 3], 'an is copy topic is not written back';

my @r = 1, 2;
@r.map(-> $x is rw { $x++ }).eager;
is-deeply @r, [2, 3], 'is rw still writes back';
