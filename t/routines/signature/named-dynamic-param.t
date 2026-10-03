# A named dynamic parameter (`:$*x`) is visible to callees as `$*x`, and its
# `named_names` is the argument name without the twigil. Found via
# Test::Describe (`it -> :$bla, :$*bli { test-bli }`).
use Test;

plan 6;

sub inner { $*x }
sub outer-named(:$*x) { inner() }
is outer-named(:x(5)), 5, 'a named $*x parameter is seen by a callee';

sub outer-pos($*x) { inner() }
is outer-pos(6), 6, 'as is a positional one';

my &blk = -> :$a, :$*x { inner() };
is blk(:a(1), :x(7)), 7, 'and one on a pointy block';

is-deeply &blk.signature.params[1].named_names, ('x',), 'named_names drops the * twigil';
is &blk.signature.params[1].name, '$*x', 'the parameter name keeps it';

my %args = x => 8;
my %wanted is Set = &blk.signature.params.grep(*.named).map(|*.named_names);
is blk(|%args.grep({ %wanted{.key} }).Hash), 8, 'filtering arguments by named_names reaches the parameter';
