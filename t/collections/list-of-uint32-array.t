use Test;

# `.list` of a native array is a List of its elements, not the array itself.
plan 10;

my uint32 @b = 1, 2;
is @b.list.WHAT.^name, 'List', 'uint32 array .list is a List';
is @b.list.raku, '(1, 2)', 'with its elements';
is-deeply @b.list, (1, 2), 'is-deeply against a List';
my int @i = 3, 4;
is @i.list.WHAT.^name, 'List', 'int array';
my num @n = 1e0, 2e0;
is @n.list.raku, '(1e0, 2e0)', 'num array';
my str @s = "a", "b";
is @s.list.WHAT.^name, 'List', 'str array';

my Int @e = 1, 2;
is @e.list.WHAT.^name, 'Array', 'a boxed typed Array is its own list';
my @p = 1, 2;
is @p.list.WHAT.^name, 'Array', 'a plain Array too';
is @b.elems, 2, 'the native array itself is untouched';
@b.push(3);
is @b.list.elems, 3, 'and a later push shows in a new list';
