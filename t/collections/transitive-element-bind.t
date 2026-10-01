use Test;

# Binding a scalar to a variable that is itself bound to an element
# (`my $q := @a[2]; my $n := $q`) must share the element's container
# transitively: writes through either name reach the array slot (#10530).

plan 10;

{
    my @a = 1, 2, 3;
    my $q := @a[2];
    my $n := $q;
    $n = 30;
    is-deeply @a, [1, 2, 30], 'write through the second-hop name reaches the element';
    is $q, 30, 'and the first-hop name sees it';
}

{
    my @b = 1, 2, 3;
    my \r = @b[1];
    my $m := r;
    $m = 9;
    is-deeply @b, [1, 9, 3], 'bind to a sigilless element alias shares the element';
}

{
    my @c = 1, 2, 3;
    my $q := @c[2];
    my $n := $q;
    $q = 7;
    is $n, 7, 'write through the first-hop name is seen by the second';
    @c[2] = 11;
    is $n, 11, 'an element store is seen through the chain';
}

{
    my %h = a => 1;
    my $x := %h<a>;
    my $y := $x;
    $y = 5;
    is-deeply %h, {a => 5}, 'hash element bind chains the same way';
}

{
    my @a = 1, 2, 3;
    my $q := @a[0];
    my $r := $q;
    my $s := $r;
    $s = 10;
    is-deeply @a, [10, 2, 3], 'a three-hop chain still reaches the element';
}

{
    my @a = 1, 2, 3;
    my $u := @a[1];
    my $v := $u;
    my &f = { $v = 77 };
    f();
    is-deeply @a, [1, 77, 3], 'a closure write through the chained name reaches the element';
}

{
    sub g(@z) { my $e := @z[0]; my $w := $e; $w = 'g' }
    my @d = 1, 2;
    g(@d);
    is-deeply @d, ['g', 2], 'chained element bind inside a routine writes to the caller array';
}

{
    # The BinaryHeap insertion walk: `$node := parent` rebinds the walker to
    # the parent element's container.
    my @heap;
    my $elems = 0;
    sub insert(\value) {
        my $pos = $elems;
        my $node := @heap[$elems++];
        $node = value;
        while $pos > 0 && value > my \parent = @heap[$pos = ($pos - 1) div 2] {
            $node = parent;
            $node := parent;
        }
        $node = value;
    }
    insert($_) for 1, 2, 3, 17, 19, 36, 7, 25, 100;
    is-deeply @heap, [100, 36, 19, 25, 3, 2, 7, 1, 17], 'heap sift-up via rebinding element aliases';
}
