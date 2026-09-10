use Test;

plan 27;

class SortedArray is Array {
    method iterator() { self.sort.iterator }
}
class PlainArray is Array { }
class PlainList  is List  { }

# `Array.new`'s one-argument rule reaches an `is Array` subclass: a single
# non-itemized Positional argument spreads into its elements.
{
    my @thing := SortedArray.new([3, 2, 1, 4]);
    is @thing.elems, 4, 'a subclass constructor spreads a single Positional argument';
    is-deeply @thing.map({ $_ }).List, (1, 2, 3, 4),
        'the ticket repro: the iterator override orders the elements';

    is PlainArray.new([3, 2, 1, 4]).elems, 4, 'the same rule with no override';
    is PlainArray.new((1, 2), (3, 4)).elems, 2, 'two arguments each stay whole';
    is PlainArray.new(1).elems, 1, 'a single non-Positional argument is one element';
    is PlainList.new([3, 2, 1, 4]).elems, 1, 'List.new is **@ and never spreads';
}

# An `is Array` subclass instance is Positional: it decomposes into its
# elements rather than being iterated as one item.
{
    my @thing := PlainArray.new(3, 2, 1, 4);
    my @seen;
    @seen.push($_) for @thing;
    is-deeply @seen.List, (3, 2, 1, 4), 'for iterates the elements';
    is-deeply (|@thing).List, (3, 2, 1, 4), '| slips the elements';
    my @copy = @thing;
    is-deeply @copy.List, (3, 2, 1, 4), 'list assignment distributes the elements';

    my @slice;
    @slice[^4] = PlainArray.new(3, 2, 1, 4);
    is-deeply @slice.List, (3, 2, 1, 4), 'a slice assignment distributes the elements';
    my @short;
    @short[0, 1] = PlainArray.new(3, 2, 1, 4);
    is-deeply @short.List, (3, 2), 'a short slice takes the leading elements';
}

# The methods raku defines through the Iterable protocol follow the override.
{
    my @thing := SortedArray.new(3, 2, 1, 4);
    my @seen;
    @seen.push($_) for @thing;
    is-deeply @seen.List, (1, 2, 3, 4), 'for follows the override';
    is-deeply @thing.map({ $_ }).List, (1, 2, 3, 4), 'map follows the override';
    is-deeply @thing.grep({ $_ > 0 }).List, (1, 2, 3, 4), 'grep follows the override';
    # `Array.list` returns `self`, so compare its rendering rather than
    # re-coercing (which would re-read the storage).
    is @thing.list.raku, '[1, 2, 3, 4]', 'list follows the override';
    is-deeply @thing.Seq.List, (1, 2, 3, 4), 'Seq follows the override';
    is @thing.eager.raku, '[1, 2, 3, 4]', 'eager follows the override';
    is-deeply @thing.flat.List, (1, 2, 3, 4), 'flat follows the override';
    is @thing.raku, '[1, 2, 3, 4]', 'raku follows the override';
    is @thing.gist, '[1 2 3 4]', 'gist follows the override';
    my @copy = @thing;
    is-deeply @copy.List, (1, 2, 3, 4), 'list assignment follows the override';
}

# The index-and-length methods answer from the reified storage, NOT the
# override -- measured against rakudo.
{
    my @thing := SortedArray.new(3, 2, 1, 4);
    is @thing[0], 3, 'a subscript reads the storage, not the override';
    is @thing.elems, 4, 'elems counts the storage';
    is @thing.join(','), '3,2,1,4', 'join reads the storage';
    is @thing.head, 3, 'head reads the storage';
    is-deeply (|@thing).List, (3, 2, 1, 4), '| slips the storage, not the override';
}

# An `is List` subclass honours the override too.
{
    class SortedList is List {
        method iterator() { self.sort.iterator }
    }
    my @l := SortedList.new(3, 2, 1, 4);
    my @seen;
    @seen.push($_) for @l;
    is-deeply @seen.List, (1, 2, 3, 4), 'an is List subclass follows the override';
}
