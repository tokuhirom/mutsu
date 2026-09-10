use Test;

# `class C is List { }` gets `List.new`'s positional constructor and answers
# positional methods from the elements, exactly as `class C is Array { }` does.
# Cro's `Cro::HTTP::MultiValue is List does Stringy` is built that way, so a
# query string or form body with a repeated key could not be represented at all.

plan 8;

class MyList is List { }

{
    my $l = MyList.new('a', 'b', 'c');
    is $l.^name, 'MyList', 'the instance keeps its own type';
    is $l.elems, 3, '.elems counts the positional constructor arguments';
    is $l.join('-'), 'a-b-c', '.join reaches the elements';
    is $l[1], 'b', 'positional indexing reaches the elements';
    is $l.map(*.uc).join(','), 'A,B,C', 'iteration reaches the elements';
}

# A List subclass is immutable, unlike an Array subclass.
{
    my $l = MyList.new(1, 2);
    dies-ok { $l.push(3) }, 'a List subclass rejects .push';
    is $l.elems, 2, 'and is unchanged by the attempt';
}

# `is Array` still gets a mutable backing store.
{
    class MyArray is Array { }
    my $a = MyArray.new(1, 2);
    $a.push(3);
    is $a.elems, 3, 'an Array subclass still accepts .push';
}
