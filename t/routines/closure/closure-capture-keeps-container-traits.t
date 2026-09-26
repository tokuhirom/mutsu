use Test;

# An escaping closure (a closure literal passed as a call argument, or stored)
# holds the locals it captures and mutates in shared `ContainerRef` cells.
# Every write through such a cell must honour the same container traits the
# variable's own store path does (#9488): `is default`, `is Map`, element
# types, shapes, QuantHash semantics. Each case below runs through an
# escaping closure, in both the expression and the statement call form.

plan 23;

sub run(&c) { c() }

{
    my %m is Map = a => 42;
    my $r = dies-ok { %m<a> = 666 }, 'Map element store through a cell dies';
    dies-ok { %m<a> = 666 }, '... statement form';
    is %m<a>, 42, '... and the Map is unchanged';
    dies-ok { %m = b => 1 }, 'Map reinitialization through a cell dies';
}

{
    my $a is default(42) = 1;
    my $r = lives-ok { $a = Nil }, 'Nil into an is-default scalar through a cell';
    is $a, 42, '... restores the default';
    $a = 1;
    lives-ok { $a = Nil }, '... statement form';
    is $a, 42, '... restores the default';
}

{
    subset Y of Int where 1..10;
    my Y @x;
    @x.push: 10;
    throws-like { @x[0]++ }, X::TypeCheck, 'element ++ through a cell is type checked';
    is @x[0], 10, '... and the element is unchanged';
}

{
    my Int @a;
    run { @a[4]++ };
    is @a.gist, '[(Int) (Int) (Int) (Int) 1]', 'holes autovivified through a cell keep the element type';
}

{
    my %h = a => 1;
    my &read = sub { %h };
    run { %h{'c', 'd'} = 3, 4 };
    is-deeply %h, { a => 1, c => 3, d => 4 }, 'hash slice through a cell keeps the other keys';
    is-deeply read(), %h, '... and stays in the shared cell';
}

{
    my @a[3, 3];
    lives-ok { @a[2;1] = 1 }, 'in-bounds multi-dim store through a cell';
    dies-ok { @a[3;1] = 1 }, 'out-of-bounds multi-dim store through a cell dies';
    is @a.shape, (3, 3), '... and the shape is unchanged';
}

{
    my $b = <a>.Bag;
    throws-like { $_ = 666 for $b.values }, Exception, 'Bag .values stays immutable through a cell';
    throws-like { for $b.kv -> \k, \v { v = 22 } }, X::Assignment::RO,
      'Bag .kv stays immutable through a cell';
}

{
    my %b is Bag = <a b>;
    dies-ok { %b = <e f g> }, 'Bag reinitialization through a cell dies';
    is %b.keys.sort.join(' '), 'a b', '... and the Bag is unchanged';
}

{
    my $b = <a>.BagHash;
    run { $_ = 7 for $b.values };
    is $b<a>, 7, 'BagHash .values write-back through a cell';
}

{
    my $s = SetHash[Str].new(<a b c>);
    throws-like { $s{42} = 1 }, X::TypeCheck::Binding,
      'parameterized SetHash key type is checked through a cell';
}

{
    my %h = :1a, :2b;
    run { %h<c> = 3 };
    isa-ok %h<a>.VAR, Scalar, 'hash values stay containerized through a cell';
}
