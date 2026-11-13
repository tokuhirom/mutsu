use Test;

# `my $x := @a.AT-POS($i)` binds the element's container, exactly like
# `my $x := @a[$i]`: a missing element is autovivified on the first
# assignment through the binding. BinaryHeap's `!insert` does this on an
# attribute array (`my $node := @!array.AT-POS($!elems); $node = value`).

plan 6;

{
    my @a;
    my $n := @a.AT-POS(2);
    $n = 7;
    is @a.raku, '[Any, Any, 7]', 'binding a missing element autovivifies it on assignment';
}

{
    my @b = 1;
    my $m := @b.AT-POS(0);
    $m = 9;
    is @b.raku, '[9]', 'binding an existing element writes through to it';
}

{
    my @c;
    my $p := @c.AT-POS(0);
    is @c.elems, 0, 'binding alone does not autovivify the element';
}

class Heap {
    has @!array;
    has int $!elems;
    method insert(Mu \value) {
        my $node := @!array.AT-POS($!elems);
        $node = value;
        $!elems++;
        self
    }
    method values { @!array[^$!elems] }
}

is Heap.new.insert(3).insert(5).values.raku, '(3, 5)',
    'an attribute array element bound through AT-POS is assigned';
is Heap.CREATE.insert(4).values.raku, '(4,)',
    'the same on an object made by CREATE';

{
    my @d = 1, 2;
    my $q := @d.AT-POS(1);
    $q := 10;
    is @d.raku, '[1, 2]', 'rebinding the variable leaves the element alone';
}
