use Test;

# `.Array` yields a real Array (renders [...] and `.WHAT` is Array);
# `.list` yields a List. Previously `.Array` on a Range / Hash / Set / Buf /
# scalar produced a List.

plan 18;

# Range -> Array
is (1..3).Array.WHAT.^name, 'Array', 'Range.Array is an Array';
is (1..3).Array.raku, '[1, 2, 3]', 'Range.Array renders [1, 2, 3]';
is (1..^4).Array.raku, '[1, 2, 3]', 'half-open Range.Array';
is (1^..4).Array.raku, '[2, 3, 4]', 'exclusive-start Range.Array';

# Range -> list stays a List
is (1..3).list.WHAT.^name, 'List', 'Range.list is a List';
is (1..3).list.raku, '(1, 2, 3)', 'Range.list renders (1, 2, 3)';

# Hash -> Array
{
    my %h = a => 1;
    is %h.Array.WHAT.^name, 'Array', 'Hash.Array is an Array';
    is %h.Array.raku, '[:a(1)]', 'Hash.Array renders [:a(1)]';
}

# Set -> Array
is set(1).Array.WHAT.^name, 'Array', 'Set.Array is an Array';

# scalar -> Array (single element)
is 42.Array.WHAT.^name, 'Array', 'Int.Array is an Array';
is 42.Array.raku, '[42]', 'Int.Array renders [42]';

# List -> Array
is (1, 2, 3).Array.WHAT.^name, 'Array', 'List.Array is an Array';
is (1, 2, 3).Array.raku, '[1, 2, 3]', 'List.Array renders [1, 2, 3]';

# List.list stays a List
is (1, 2, 3).list.WHAT.^name, 'List', 'List.list is a List';

# word list -> Array
is <a b c>.Array.WHAT.^name, 'Array', 'word list .Array is an Array';
is <a b c>.Array.raku, '["a", "b", "c"]', 'word list .Array renders';

# binding to @-var
{
    my @a = (1..3).Array;
    is @a.raku, '[1, 2, 3]', 'Range.Array assigned to @a';
}

# Seq -> Array
is (1, 2, 3).Seq.Array.WHAT.^name, 'Array', 'Seq.Array is an Array';
