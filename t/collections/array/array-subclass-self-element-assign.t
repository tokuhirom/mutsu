use Test;

# `.self` answers the invocant itself, including on an `is Array` / `is Hash`
# subclass instance, and an element store through a method result that is
# such an instance writes into that instance (#9168).

plan 9;

class A is Array {}
class H is Hash {}

{
    my $a = A.new;
    is $a.self.^name, 'A', '.self on an is Array instance is the instance';
    $a.self[1] = 1;
    is $a.gist, '[(Any) 1]', '$a.self[1] = 1 stores into the instance';
    is $a.self[1], 1, '.self[1] reads it back';
    ok $a.self === $a, '.self is identical to the invocant';
}

{
    my $h = H.new;
    $h.self<k> = 3;
    is $h<k>, 3, '$h.self<k> = 3 stores into an is Hash instance';
    is $h.self.^name, 'H', '.self on an is Hash instance is the instance';
}

{
    class C { has $.arr = A.new; has $.hsh = H.new }
    my $c = C.new;
    $c.arr[0] = 7;
    is-deeply $c.arr.List, (7,), 'store through a read-only accessor returning an is Array object';
    $c.hsh<x> = 8;
    is $c.hsh<x>, 8, 'store through a read-only accessor returning an is Hash object';
}

{
    my @b = 1, 2;
    @b.self[0] = 9;
    is-deeply @b, [9, 2], '@b.self[0] = 9 on a plain array';
}
