use Test;

# Math::Sequences style self-referential sequences: a closure created by the
# initializer of `my @a = ...` / `our @a = ...` reads the declared array, not a
# snapshot taken before the declaration's store.
plan 4;

my @h = 0, {@h[$++] + 1} … *;
is @h[^4].List, (0, 1, 2, 3), 'my @h';

our @o = 0, {@o[$++] + 1} … *;
is @o[^4].List, (0, 1, 2, 3), 'our @o';

sub f { my @s = 1, {@s[$++] * 2} … *; @s[^5].List }
is f(), (1, 2, 4, 8, 16), 'inside a sub';

my @n = 0, { my $c = { @n[0] }; @n[$++] + 1 + $c() * 0 } … *;
is @n[^3].List, (0, 1, 2), 'a closure nested in the initializer closure';
