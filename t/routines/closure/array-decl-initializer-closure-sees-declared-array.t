use Test;

# Math::Sequences style self-referential sequences: a closure created by the
# initializer of `my @a = ...` / `our @a = ...` reads the declared array, not a
# snapshot taken before the declaration's store.
plan 8;

my @h = 0, {@h[$++] + 1} … *;
is @h[^4].List, (0, 1, 2, 3), 'my @h';

our @o = 0, {@o[$++] + 1} … *;
is @o[^4].List, (0, 1, 2, 3), 'our @o';

sub f { my @s = 1, {@s[$++] * 2} … *; @s[^5].List }
is f(), (1, 2, 4, 8, 16), 'inside a sub';

my @n = 0, { my $c = { @n[0] }; @n[$++] + 1 + $c() * 0 } … *;
is @n[^3].List, (0, 1, 2), 'a closure nested in the initializer closure';

# #18248 shape B (ADR-12529 phase 3): the closure shares the declared
# container itself, not one its caller's frame happens to hold. Each
# expectation was checked against rakudo 2026.11.
sub h { my %h = a => 1, b => { %h<a> + 1 }; %h<b>() }
is h(), 2, 'a hash declaration\'s initializer closure reads the hash';

my @per-iteration;
for 1, 2 -> $i { my @s = $i, { @s[0] * 10 }; @per-iteration.push(@s[1]) }
is @per-iteration.map({ $_() }).List, (10, 20), 'every iteration\'s closure reads its own array';

sub later { my @s = 1, { @s.elems }; my $c = @s[1]; @s = 9; $c() }
is later(), 1, 'the closure sees a later assignment to the array';

sub shadowing { my @s = 7; { my @s = 1, { @s[0] }; @s[1]() } }
is shadowing(), 1, 'the closure reads the new array, not the one it shadows';
