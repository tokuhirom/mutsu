use v6;
use Test;

plan 10;

# An unknown method on a Code object dies X::Method::NotFound naming the
# Code's own type (#11629). `{ ... }.lazy` used to share its AST with the
# `lazy { ... }` statement prefix, so it RAN the block and called `.lazy` on
# its result (`invocant of type 'Any'`), and `(-> {}).lazy` was silent.

sub f { }

throws-like { (-> {}).frobnicate }, X::Method::NotFound,
    typename => 'Block', method => 'frobnicate', 'unknown method on a pointy block';
throws-like { (-> {}).lazy }, X::Method::NotFound,
    typename => 'Block', method => 'lazy', '.lazy on a pointy block';
throws-like { &f.lazy() }, X::Method::NotFound,
    typename => 'Sub', method => 'lazy', '.lazy on a Sub';

my $ran = 0;
throws-like { { $ran++ }.lazy() }, X::Method::NotFound,
    typename => 'Block', method => 'lazy', '.lazy on a bare block literal';
is $ran, 0, 'the block is not called';

# The `lazy BLOCK` statement prefix still runs the block and marks its result.
my @order;
my $x = lazy { @order.push: 'run'; 1, 2, 3 };
@order.push: 'after';
is-deeply @order, ['run', 'after'], 'lazy BLOCK runs the block at once';
ok $x.is-lazy, '...and its result is lazy';
is $x.eager.join(','), '1,2,3', '...with the block\'s values';

my @a = lazy { (^3).map(* * 2) };
ok @a.is-lazy, 'lazy BLOCK assigned to an array stays lazy';
is @a[1], 2, 'its elements are the block\'s';
