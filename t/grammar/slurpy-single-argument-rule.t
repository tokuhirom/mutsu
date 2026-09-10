use Test;

# `map(&code, +values)`, `grep(&matcher, +values)` and `Array.new(|c)` all slurp
# with a `+@`-shaped parameter, which applies the *single-argument rule*: exactly
# one list argument is flattened into its elements, two or more are each one
# element of their own. mutsu flattened every argument, so
# `map -> [$a, $b] {...}, (1,2), (3,4)` ran the block four times over four Ints
# instead of twice over two pairs -- the shape `Digest::RIPEMD` builds its round
# tables with.

plan 20;

# --- map ------------------------------------------------------------------

is (map { .raku }, (1, 2), (3, 4)).join('|'), '(1, 2)|(3, 4)',
    'two list arguments are two elements';
is (map { .raku }, (1, 2)).join('|'), '1|2',
    'one list argument is flattened';
is (map { .raku }, 1, 2).join('|'), '1|2',
    'two scalar arguments are still two elements';
is (map { .raku }, (1, 2), 9).join('|'), '(1, 2)|9',
    'a list and a scalar do not flatten the list';
is (map { .raku }, (1, 2).Seq, (3, 4).Seq).join('|'), '(1, 2).Seq|(3, 4).Seq',
    'Seq arguments are kept whole';
is (map { .raku }, 1 .. 3, 5 .. 7).join('|'), '1..3|5..7',
    'Range arguments are kept whole';
is (map { .raku }, [1, 2], [3, 4]).join('|'), '[1, 2]|[3, 4]',
    'Array arguments are kept whole';
is (map { .elems }, (1, 2), ()).join('|'), '2|0',
    'an empty list argument is still one element';

{
    my @a = 1, 2;
    is (map { .raku }, @a, @a).join('|'), '[1, 2]|[1, 2]',
        'two @-variables are two elements';
    is (map { .raku }, @a).join('|'), '1|2',
        'one @-variable is flattened';
}

is (map { .raku }, (1, 2) xx 2).join('|'), '(1, 2)|(1, 2)',
    'a single Seq-of-lists argument flattens one level only';

# --- destructuring, the shape this was found through -----------------------

is (map -> [$a, $b] { "$a/$b" }, (1, 2), (3, 4)).join('|'), '1/2|3/4',
    'a destructuring block binds each list argument as a whole';

# --- grep -----------------------------------------------------------------

is (grep { True }, (1, 2), (3, 4)).elems, 2, 'grep counts two list arguments as two';
is (grep { True }, (1, 2)).elems, 2, 'grep flattens a single list argument';
is (grep { $_ > 1 }, 1, 2, 3).join(','), '2,3', 'grep over scalars is unchanged';

# --- Array.new ------------------------------------------------------------

is Array.new((1, 2)).elems, 2, 'Array.new flattens a single list argument';
is Array.new([1, 2]).elems, 2, '... and a single Array argument';
is Array.new((1, 2), (3, 4)).elems, 2, '... but keeps two arguments as two elements';
is Array.new($(1, 2)).elems, 1, '... and never unwraps an itemized argument';
is List.new((1, 2)).elems, 1, 'List.new slurps with **@ and does not apply the rule';
