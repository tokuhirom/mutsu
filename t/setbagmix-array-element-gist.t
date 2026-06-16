use Test;

# A Set/Bag/Mix that sits *as* an element of a list/array/seq keeps its
# type-name wrapper in `.gist` (`Set(1 2 3)`), like the say/gist fast path,
# rather than collapsing to its bare-element `.Str` form (`1 2 3`). The
# explicit `.gist` method previously diverged from `say`'s implicit gist here.

plan 10;

is [set(1, 2, 3)].gist, '[Set(1 2 3)]', 'Set element in array .gist';
is (set(1, 2, 3),).gist, '(Set(1 2 3))', 'Set element in list .gist';
is [bag(1, 1, 2)].gist, '[Bag(1(2) 2)]', 'Bag element in array .gist';
is (bag("a", "a", "b"),).gist, '(Bag(a(2) b))', 'Bag element in list .gist';
is [set(1, 2), set(3, 4)].gist, '[Set(1 2) Set(3 4)]', 'multiple Set elements';
is (set("x"),).Seq.gist, '(Set(x))', 'Set element in a Seq .gist';
is (set(1, 2),).Slip.gist, '(Set(1 2))', 'Set element in a Slip .gist';

# Mixed with other container elements (each keeps its own gist form)
is [set(1, 2), {a => 1}, (3, 4)].gist, '[Set(1 2) {a => 1} (3 4)]',
    'Set, Hash and List elements each gist correctly';

# Hash value (already correct) stays correct
is { k => set(1, 2) }.gist, '{k => Set(1 2)}', 'Set as a Hash value .gist';

# The implicit say-gist and the explicit .gist method agree
{
    my @a = (set(1, 2, 3),);
    is @a.gist, '[Set(1 2 3)]', 'explicit .gist matches the say fast path';
}
