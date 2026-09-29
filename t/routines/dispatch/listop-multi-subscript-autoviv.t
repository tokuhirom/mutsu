use Test;

# ADR-0044 D3: a user `multi push` in scope must not stop `push(@a[2], ...)`
# from falling back to the core candidate and autovivifying the slot.

plan 4;

multi push(Str $x, *@) { "str" }

my @a;
push(@a[2], 1);
is-deeply @a, [Any, Any, [1]], 'push(@a[2], 1) autovivifies with a user multi in scope';

my %h;
push(%h<k>, 1, 2);
is-deeply %h, {k => [1, 2]}, 'push(%h<k>, ...) autovivifies with a user multi in scope';

my @b = "abc";
is push(@b[0], 1), "str", 'a defined Str slot still reaches the user candidate';

my @c = [[1],];
push(@c[0], 2);
is-deeply @c, [[1, 2],], 'an existing Array slot is pushed into';
