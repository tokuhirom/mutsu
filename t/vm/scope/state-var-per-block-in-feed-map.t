use v6;
use Test;

plan 8;

# PLAN §8.14: two distinct blocks that each declare `state $n`, invoked via the
# feed / function form of map, must NOT share state storage (the inline map
# path re-compiles the block, so the compile-time state key alone collides).

my @a = <a b c d e>;
@a ==> map({ state $n = -1; $n++; $_ => $n.clone }) ==> my %first;
my @b = <H He Li B>;
@b ==> map({ state $n = 0; $n++; $_ => $n.clone }) ==> my %second;
is %second<H>, 1, 'the second feed-map block re-runs its own state initializer';
is %first<a>, 0, 'first block starts from its own initializer';
is %first<e>, 4, 'first block counts its own items';
is %second<B>, 4, 'second block counts its own items';

# The function form with distinct blocks is likewise independent.
my %f1 = map({ state $n = -1; $n++; $_ => $n.clone }, <a b>);
my %f2 = map({ state $n = 0; $n++; $_ => $n.clone }, <x y>);
is %f2<x>, 1, 'function-form map keeps per-block state';

# State still persists across items WITHIN one map.
my @counts = map({ state $n = 0; $n++; $n }, <p q r>);
is-deeply @counts, [1, 2, 3], 'state persists across the items of one map';

# grep and first blocks with state are scoped per block too.
my @g1 = grep({ state $n = 0; $n++; $n > 1 }, <a b c>);
my @g2 = grep({ state $n = 0; $n++; $n > 1 }, <x y z>);
is-deeply @g2, [<y z>], 'a second grep block re-runs its own state initializer';
my $h1 = first({ state $n = 0; $n++; $n == 2 }, <a b c>);
my $h2 = first({ state $n = 0; $n++; $n == 2 }, <x y z>);
is $h2, 'y', 'a second first block re-runs its own state initializer';

done-testing;
