use Test;

# Attribute and collection CONTAINER identity.
#
# Every assertion here is about which *container* a value lives in, not about
# which value it holds: a value-only check would pass on a copy and miss the
# whole point. `=:=` on an index expression can itself promote the element to a
# shared cell, so identity is asserted in BOTH operand orders wherever an
# index/accessor expression is involved -- a test that only checks one order can
# pass by luck.

plan 60;

# ---------------------------------------------------------------------------
# Mu.clone shares Array/Hash attribute containers, copies scalar ones
# ---------------------------------------------------------------------------
{
    class CloneMe {
        has $.foo is rw = 42;
        has @.bar       = <a b>;
        has %.baz       = <a b c d>;
    }

    my $o1 = CloneMe.new;
    my $o2 = $o1.clone;

    ok $o1.bar =:= $o2.bar, 'clone shares the @-attribute container';
    ok $o2.bar =:= $o1.bar, 'clone shares the @-attribute container (reversed)';
    ok $o1.baz =:= $o2.baz, 'clone shares the %-attribute container';
    ok $o2.baz =:= $o1.baz, 'clone shares the %-attribute container (reversed)';

    $o2.bar.push('Z');
    is $o1.bar.join(','), 'a,b,Z', 'push through the clone is visible on the original';
    $o2.baz<x> = 'y';
    is $o1.baz<x>, 'y', 'hash store through the clone is visible on the original';

    # A whole-container assignment through the accessor is a list assignment
    # INTO the existing container, so the sharing survives it.
    $o2.bar = <Z Y>;
    ok $o1.bar =:= $o2.bar, 'accessor list-assign keeps the container identity';
    ok $o2.bar =:= $o1.bar, 'accessor list-assign keeps the container identity (reversed)';
    is $o1.bar.join(','), 'Z,Y', 'accessor list-assign is visible on the original';

    $o2.baz = (q => 1);
    ok $o1.baz =:= $o2.baz, 'accessor hash-assign keeps the container identity';
    is $o1.baz.keys.join(','), 'q', 'accessor hash-assign is visible on the original';

    # A scalar attribute gets its own container on clone.
    $o2.foo = 70;
    is $o1.foo, 42, 'clone gives a $-attribute its own container';
    is $o2.foo, 70, 'the clone sees its own $-attribute value';

    # An explicit override in .clone builds a NEW container.
    my $o3 = CloneMe.new;
    my $o4 = $o3.clone(bar => <n m>);
    nok $o3.bar =:= $o4.bar, 'clone(:attr) does not share the container';
    nok $o4.bar =:= $o3.bar, 'clone(:attr) does not share the container (reversed)';
    is $o3.bar.join(','), 'a,b', 'clone(:attr) leaves the original container alone';
}

# Two instances never share an attribute container.
{
    class Fresh { has @.x = 1, 2; has %.h = a => 1 }
    my $a = Fresh.new;
    my $b = Fresh.new;
    nok $a.x =:= $b.x, 'each instance gets its own @-attribute container';
    nok $a.h =:= $b.h, 'each instance gets its own %-attribute container';
    $a.x = (7, 8);
    is $b.x.join(','), '1,2', 'assigning one instance leaves the other alone';
}

# An object OWNS its @/% attribute containers: a supplied argument is assigned
# INTO the attribute's own container, never adopted wholesale.
{
    class Owned { has @.x; has %.h }
    my @src  = 1, 2;
    my %hsrc = a => 1;
    my $o = Owned.new(x => @src, h => %hsrc);
    nok $o.x =:= @src, 'a supplied @ argument is copied into the attribute container';
    nok @src =:= $o.x, 'a supplied @ argument is copied into the attribute container (reversed)';
    nok $o.h =:= %hsrc, 'a supplied % argument is copied into the attribute container';
    $o.x.push(9);
    $o.h<z> = 1;
    is @src.join(','), '1,2', 'pushing through the attribute cannot reach the caller array';
    is %hsrc.keys.join(','), 'a', 'storing through the attribute cannot reach the caller hash';
    $o.x = (7,);
    is @src.join(','), '1,2', 'accessor list-assign cannot reach the caller array either';
}

# ---------------------------------------------------------------------------
# `.head`/`.tail` as an lvalue mutate the container, whoever holds it
# ---------------------------------------------------------------------------
{
    class Tailed {
        has @!numbers;
        has @.pub = 'p';
        method go() {
            @!numbers.push: '';
            @!numbers.tail ~= 'x';
            @!numbers.join(',');
        }
        method priv-container-stable() {
            @!numbers.push: 'a';
            my $before = @!numbers;
            @!numbers.tail ~= 'b';
            $before === @!numbers;
        }
    }

    is Tailed.new.go, 'x', '.tail lvalue mutates a private attribute array';
    ok Tailed.new.priv-container-stable,
        '.tail lvalue keeps the private attribute container identity';

    my $t = Tailed.new;
    my $held = $t.pub;
    $t.pub.tail ~= 'q';
    is $t.pub.join(','), 'pq', '.tail lvalue mutates a public attribute array';
    ok $held === $t.pub, '.tail lvalue keeps the public attribute container identity';
    ok $t.pub === $held, '.tail lvalue keeps the public attribute container identity (reversed)';
}

{
    my @a = 1, 2, 3;
    my $before = @a;
    @a.tail ~= 5;
    is @a.join(','), '1,2,35', '.tail lvalue still works on a plain lexical array';
    ok $before === @a, '.tail lvalue keeps a plain lexical array container identity';
    @a.head = 9;
    is @a.join(','), '9,2,35', '.head lvalue assigns into the container';
    ok @a === $before, '.head lvalue keeps the container identity (reversed)';
}

# A closure that captured the array sees a `.tail` write through the container.
{
    my @c = 'a', 'b';
    my $peek = { @c.join(',') };
    @c.tail ~= 'z';
    is $peek(), 'a,bz', '.tail lvalue is visible through a captured container';
}

# ---------------------------------------------------------------------------
# .Capture reads public ACCESSORS, not the attribute store
# ---------------------------------------------------------------------------
{
    class Captured {
        has $.foo = 42;
        has $.bar = 70;
        has $!hidden = 9;
        method bar { 'something else' }
    }
    my %c = Captured.new.Capture.hash;
    is %c<bar>, 'something else', '.Capture dispatches an overriding accessor';
    is %c<foo>, 42, '.Capture reports the plain accessor value';
    nok %c<hidden>:exists, '.Capture does not leak a private attribute';
    is %c.keys.sort.join(','), 'bar,foo', '.Capture exposes exactly the public attributes';
}

# ---------------------------------------------------------------------------
# `.item` denotes the variable's OWN container -- it never copies
# ---------------------------------------------------------------------------
{
    my $x = 1;
    ok $x.item =:= $x, '.item returns the variable container itself';
    ok $x =:= $x.item, '.item returns the variable container itself (reversed)';
}

# A List holds the containers it is built from, `.item`-ed or not.
{
    my $a = 1;
    my $l = ($a.item, 2);
    ok $l[0] =:= $a, 'a List built from .item holds the container';
    ok $a =:= $l[0], 'a List built from .item holds the container (reversed)';
    $a = 5;
    is $l[0], 5, 'mutating the variable is visible through the List';
}

{
    my $a = 1;
    my $l = ($a, 2);
    ok $l[0] =:= $a, 'a List built from a bare variable holds the container';
    ok $a =:= $l[0], 'a List built from a bare variable holds the container (reversed)';
}

# The traps.rakudoc Fibonacci "trap": every pushed tuple aliases $a/$b.
{
    my @arr;
    my ($a, $b) = (1, 1);
    for ^3 {
        ($a, $b) = ($b, $a + $b);
        @arr.push: ($a.item, $b.item);
    }
    is @arr.elems, 3, 'three tuples were pushed';
    is @arr».join('-').join(' '), '3-5 3-5 3-5',
        'each pushed tuple still aliases the source containers';
}

# A Pair literal written directly as an argument is data, and its value keeps
# its container; `.clone` is what breaks the alias.
{
    my @a;
    my @cloned;
    my $i = 0;
    for 1 .. 3 {
        $i++;
        @a      .push: "k$i" => $i;
        @cloned .push: "k$i" => $i.clone;
    }
    is @a».value.join(','), '3,3,3', 'a pushed Pair aliases its value container';
    is @cloned».value.join(','), '1,2,3', '.clone on the value breaks the alias';
    ok @a[0].value =:= $i, 'the pushed Pair value IS the source container';
    ok $i =:= @a[0].value, 'the pushed Pair value IS the source container (reversed)';
}

# But a scalar pushed on its own is copied into the element -- an Array element
# is its own container.
{
    my $x = 1;
    my @p;
    @p.push($x);
    nok @p[0] =:= $x, 'pushing a bare scalar copies it into a fresh element';
    nok $x =:= @p[0], 'pushing a bare scalar copies it into a fresh element (reversed)';
    $x = 5;
    is @p[0], 1, 'the pushed element keeps its own value';
    is @p[0].VAR.^name, 'Scalar', 'an Array element is a Scalar container';
}

{
    my $y = 1;
    my @q = 0;
    @q[0] = $y;
    nok @q[0] =:= $y, 'element assignment copies rather than aliases';
    nok $y =:= @q[0], 'element assignment copies rather than aliases (reversed)';
}

# A Hash value is a container of its own too.
{
    my $z = 1;
    my %h = a => $z;
    is %h<a>.VAR.^name, 'Scalar', 'a Hash value is a Scalar container';
    $z = 5;
    is %h<a>, 1, 'a hash-constructor value is copied into the hash element';
}

done-testing;
