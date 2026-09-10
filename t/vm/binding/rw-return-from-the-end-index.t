# A from-the-end subscript (`*-1`, `*-0`) in CONTAINER context must yield the
# element's container, not its value.
#
# Every rvalue index path resolves a `WhateverCode` subscript against the
# subscripted array's length before using it. The two container-producing paths
# -- `:=` bind and an `is rw` routine's `return-rw <subscript>` tail -- did not:
# the index stayed an unconvertible `WhateverCode`, so the autovivifying op
# fell through to a plain READ and handed the caller the element's value.
#
# `sub g(\c) is rw { return-rw c[*-1] }; g(@a) = 9` therefore died with "Cannot
# modify an immutable Int", and `c[*-0]` on an empty array -- the append idiom
# `Crane::In` is built on, which `Config::TOML` uses for every `[[table]]`
# entry (GH #7539) -- silently wrote nowhere.
#
# Every expectation below was measured against rakudo (v2026.07).
use Test;

plan 8;

sub last-elem(\c) is rw { return-rw c[*-1] }
sub end-elem(\c) is rw { return-rw c[*-0] }
sub nth(\c) is rw { return-rw c[1] }

# `*-0` is "one past the last", i.e. an append.
my @a;
end-elem(@a) = 42;
is-deeply @a, [42], 'return-rw c[*-0] appends to an empty array';

end-elem(@a) = 43;
is-deeply @a, [42, 43], 'return-rw c[*-0] appends again';

my @b = 1, 2;
last-elem(@b) = 9;
is-deeply @b, [1, 9], 'return-rw c[*-1] writes the last element';

# The plain-integer index was never broken; it is here so a regression that
# "fixes" the WhateverCode case by breaking this one is caught too.
my @c = 1, 2, 3;
nth(@c) = 7;
is-deeply @c, [1, 7, 3], 'return-rw c[1] still writes that element';

# Through a hash element, the shape `Crane::In` actually descends into.
my %h;
%h<xs> = [];
end-elem(%h<xs>) = 'first';
end-elem(%h<xs>) = 'second';
is-deeply %h<xs>, ['first', 'second'], 'return-rw c[*-0] through a hash element appends';

# Reading through the same routine is unaffected.
is last-elem(@c), 3, 'return-rw c[*-1] still reads the last element';

# The `:=` twin of the same subscript.
my @d = 10, 20, 30;
my $tail := @d[*-1];
is $tail, 30, 'a := bind to a from-the-end index reads the element';
$tail = 99;
is-deeply @d, [10, 20, 99], 'a := bind to a from-the-end index writes through';

# vim: expandtab shiftwidth=4
