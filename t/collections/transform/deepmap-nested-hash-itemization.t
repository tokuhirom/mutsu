use Test;

# `deepmap` itemizes what a DESCEND returns, exactly as it does for a sublist.
# The Hash arm dropped the flag, so a mapped copy's nested hashes came back as
# bare values -- nothing could be bound to one, which is how
# `Crane::At.at($root, @path){$step}:delete` on a `container.deepmap({ .clone })`
# copy deleted from a temporary instead of from `$root`.
#
# The itemization must be the per-holder flag on the same `HashData` (`.item`),
# not a `Scalar` wrapper: a wrapper hands back a copy nothing can mutate in place.

plan 10;

is %(:x({:a(1)})).deepmap({ $_ }).raku, '{:x(${:a(1)})}',
    'a nested Hash comes back itemized';
is %(:x({:a({:b(1)})})).deepmap({ $_ }).raku, '{:x(${:a(${:b(1)})})}',
    'and so does one two levels down';
is (1, {:a(2)}).deepmap({ $_ }).raku, '(1, ${:a(2)})',
    'a Hash element of a List is itemized too';

# A real Array parent does NOT itemize what a descend returns -- the same rule
# the sublist arm already followed.
is [1, {:a(2)}].deepmap({ $_ }).raku, '[1, {:a(2)}]',
    'a real Array parent leaves the descended Hash bare';
is [[1, 2], 3].deepmap({ $_ }).raku, '[[1, 2], 3]',
    'and leaves a sublist bare (unchanged)';
is %(:x([1, 2])).deepmap({ $_ }).raku, '{:x($[1, 2])}',
    'an Array under a Hash was already itemized (unchanged)';

# The itemized element must still share its backing store, so the mapped copy
# is a real structure that can be written into.
my %src = :why({:do(1), :x(2)});
my $copy = %src.deepmap({ .clone });
$copy<why><do>:delete;
is-deeply $copy, {:why({:x(2)})}, 'a nested delete reaches the mapped copy';
is-deeply %src, {:why({:do(1), :x(2)})}, 'and leaves the source alone';

my $copy2 = %src.deepmap({ .clone });
my $inner = $copy2<why>;
$inner<do> = 99;
is $copy2<why><do>, 99, 'a write through a read-out nested hash reaches the copy';

# duckmap/nodemap were already right; pin them so the three stay in step.
is %(:x({:a(1)})).duckmap({ $_ }).raku, '{:x(${:a(1)})}', 'duckmap itemizes too';
