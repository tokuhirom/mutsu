use Test;

# An `@` parameter inside a destructuring sub-signature accepted only an Array,
# so a `Seq` or `Range` element failed to bind even though a plain `sub f(@a)`
# takes both. Rakudo listifies a non-Positional Iterable on the way in, which is
# why `@a.^name` is `List` for a Seq and stays `Range` for a Range.
# `Digest::RIPEMD`'s `-> [&f, $r, @K, $s] {...}` binds a `Seq` at `@K`.

plan 10;

my &g = -> [@a, $b] { "{@a.^name}:{@a.elems}" };

is g([(1, 2).Seq, 9]), 'List:2', 'a Seq element binds to an @ sub-parameter';
is g([1 .. 3, 9]), 'Range:3', 'a Range element binds and keeps its type';
is g([[7, 8], 9]), 'Array:2', 'an Array element still binds';
is g([(4, 5), 9]), 'List:2', 'a List element still binds';

dies-ok { g([%(a => 1), 9]) }, 'a Hash element is still a binding error';
dies-ok { g(['ab', 9]) }, 'a Str element is still a binding error';
dies-ok { g([5, 9]) }, 'an Int element is still a binding error';

# The bound List survives being read twice — a raw Seq would be exhausted.
{
    my &h = -> [@a, $b] { @a.elems + @a.elems };
    is h([(1, 2, 3).Seq, 9]), 6, 'the listified Seq can be iterated more than once';
}

# The same through a `map` destructuring block, the shape RIPEMD uses.
is (map -> [&f, @k] { "{&f(2)}/{@k.join(',')}" },
        (* + 1, (7, 8).Seq), (* + 10, (1, 2, 3).Seq)).join('|'),
    '3/7,8|12/1,2,3', 'a Seq binds through a map destructuring block';

# A plain (non-destructured) signature was already fine; pin it.
{
    sub p(@a) { @a.elems }
    is p((1, 2).Seq), 2, 'a plain @ parameter still takes a Seq';
}
