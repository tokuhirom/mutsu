use Test;

# `.DELETE-POS` on a shaped array empties the slot but keeps every slot:
# a shaped array is fixed-size, like the `:delete` adverb already treats
# it. Only an unshaped array drops its trailing holes. mutsu#10925.

plan 9;

my @s[4];
@s.ASSIGN-POS(1, 8);
@s.DELETE-POS(1);
is @s.gist, '[(Any) (Any) (Any) (Any)]', 'every slot still prints';
is @s.elems, 4, 'and the array keeps its size';
nok @s[1]:exists, 'the deleted slot is a hole';

my @t[3];
@t[2] = 5;
is @t.DELETE-POS(2), 5, 'deleting the last slot returns its value';
is @t.raku, 'Array.new(:shape(3,), [Any, Any, Any])', 'and keeps the shape';

my @u[3];
@u[2] = 5;
@u[2]:delete;
is @u.elems, 3, 'the :delete adverb agrees';

my @a;
@a[0] = 1;
@a[1] = 2;
@a.DELETE-POS(1);
is @a.elems, 1, 'an unshaped array still drops its trailing hole';

my @b;
@b[0] = 1;
@b[1] = 2;
@b[2] = 3;
@b.DELETE-POS(1);
is @b.elems, 3, 'a hole that is not trailing stays';
is @b.gist, '[1 (Any) 3]', 'and prints as (Any)';
