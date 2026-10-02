use Test;

# The `*-POS` methods, shaped-array element stores and stores into an
# `is Array` instance write the element in place (#9157): every alias of the
# array observes the write, and a copy taken before it does not.

plan 17;

{
    my @a = 1, 2, 3;
    my @alias := @a;
    @a.ASSIGN-POS(1, 20);
    is @alias[1], 20, 'ASSIGN-POS is seen through a := alias';
    @a.BIND-POS(2, 30);
    is @alias[2], 30, 'BIND-POS is seen through a := alias';
    @a.BIND-POS(5, 7);
    is-deeply @a[3]:exists, False, 'a gap BIND-POS grows is a hole';
    is @a[5], 7, 'BIND-POS past the end stores the element';
}

{
    my @a = [1, 2], [3, 4];
    my $inner = @a[1];
    @a.ASSIGN-POS(1, 0, 9);
    is $inner[0], 9, 'multi-dim ASSIGN-POS writes into the inner array itself';
    is @a.DELETE-POS(0, 1), 2, 'multi-dim DELETE-POS returns the deleted element';
    is-deeply @a[0][1]:exists, False, 'multi-dim DELETE-POS leaves a hole';
    my @b;
    @b.ASSIGN-POS(2, 1, 5);
    is @b.raku, '[Any, Any, [Any, 5]]', 'multi-dim ASSIGN-POS autovivifies only the indexed slot';
    @b.BIND-POS(2, 0, 3);
    is @b[2][0], 3, 'multi-dim BIND-POS binds into the inner array';
}

{
    my @s[4];
    @s[$_] = $_ * 10 for ^4;
    is-deeply @s.shape, (4,), 'element stores keep a shaped array shaped';
    is @s.join(','), '0,10,20,30', 'shaped element stores land';
    @s[1]:delete;
    is-deeply @s.shape, (4,), 'an element delete keeps the shape';
    my @m[2;2];
    @m.ASSIGN-POS(0, 1, 4);
    is-deeply @m.shape, (2, 2), 'multi-dim ASSIGN-POS keeps a 2-dim shape';
    throws-like { @m.ASSIGN-POS(0, 1) }, X::NotEnoughDimensions,
        'a short ASSIGN-POS on a 2-dim shaped array still dies';
}

{
    my Int @t;
    throws-like { @t.ASSIGN-POS(0, 'x') }, X::TypeCheck,
        message => /'element of @t'/, 'ASSIGN-POS type-checks against the element type';
}

{
    my class A is Array {}
    my $x = A.new;
    sub g { $x }
    g()[$_] = $_ for ^4;
    my $copy = $x.clone;
    g()[0] = 99;
    is $x.join(','), '99,1,2,3', 'a computed-target store into an is Array instance lands';
    is $copy.join(','), '0,1,2,3', 'a clone taken before the store does not see it';
}
