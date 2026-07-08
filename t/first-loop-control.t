use v6;
use Test;

# `next`/`last` loop control inside a `.first`/`first` block. Rakudo:
#   * `next` skips the current element (treat as non-matching, keep scanning)
#   * `last` stops the scan and returns the CURRENT element as the match
# Regression for a bug where either threw X::ControlFlow (surfaced by
# Zef::Repository::Ecosystems.update using `$!mirrors.first: -> $uri { ... next ... }`).

plan 9;

# next skips an element, later one matches
is (1, 2, 3, 4).first({ next if $_ == 2; $_ > 1 }), 3, 'method .first: next skips element';

# sub form
is (first { next if $_ == 2; $_ > 1 }, 1, 2, 3, 4), 3, 'sub first: next skips element';

# :end (reversed scan) with next
is (1, 2, 3, 4).first({ next if $_ == 4; $_ > 1 }, :end), 3, '.first :end with next';

# last returns the current element (even though the block body is false for it)
is (5, 1, 2, 3).first({ last if $_ == 1; $_ > 10 }), 1, '.first: last returns current element (A)';
is (5, 6, 1, 2).first({ last if $_ == 1; $_ > 10 }), 1, '.first: last returns current element (B)';

# a normal match before `last` fires wins
is (1, 2, 3, 4).first({ last if $_ == 3; $_ > 1 }), 2, '.first: earlier normal match beats last';

# last on the very first element returns it
is (7, 8, 9).first({ last }), 7, '.first: bare last returns first element';

# next on every element => no match (undefined result)
nok (1, 2, 3).first({ next; True }).defined, '.first: next-always yields no match';

# last never fires and nothing matches => no match
nok (1, 2, 3).first({ last if $_ == 99; $_ > 10 }).defined, '.first: no match yields undefined';
