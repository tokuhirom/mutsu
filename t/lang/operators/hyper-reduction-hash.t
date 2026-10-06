use Test;

# From Concurrent::Queue: `[>>+<<]` folds Hashes key-wise like `>>+<<` does.
plan 5;

my %a = 1 => 2, 2 => 3;
my %b = 1 => 5, 2 => 1;
is-deeply ([>>+<<] %a, %b), {1 => 7, 2 => 4}, 'reduce hyper over two hashes';
my @r = %a, %b, {1 => 1};
is-deeply ([>>+<<] @r), {1 => 8, 2 => 4}, 'reduce hyper over three hashes';
is-deeply ([>>+<<] [1, 2], [3, 4]), [4, 6], 'reduce hyper over arrays';
is-deeply ([>>+<<] {a => 1}, {a => 2}), {a => 3}, 'block-literal hashes';
throws-like { [>>+<<] [1, 2], [3] }, X::HyperOp::NonDWIM, 'non-dwimmy length mismatch still dies';

done-testing;
