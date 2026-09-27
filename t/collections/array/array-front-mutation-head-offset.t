use Test;

plan 24;

# `ArrayData` keeps a head offset so `shift`/`unshift` are amortized O(1)
# (#9121). Every other mutator used to compact that offset away first (an
# O(e) memmove per call), and a multi-element `unshift`/`prepend` or a
# `splice` moved the whole tail once per inserted element (#9156). They now
# work on the live range directly. These pin that the results -- including
# the hole bitmap `:exists` reads -- are unchanged, then time the three
# shapes that were quadratic.

sub holes(@a) { (^@a.elems).map({ @a[$_]:exists ?? 1 !! 0 }).join }

{
    my @a = 1..6;
    @a.shift for ^2;
    @a.prepend(<a b c>);
    is @a.join(','), 'a,b,c,3,4,5,6', 'prepend of several elements after shifts';
    @a.unshift('x', 'y');
    is @a.join(','), 'x,y,a,b,c,3,4,5,6', 'unshift of several elements keeps their order';
    @a[1] = 'Y';
    is @a.join(','), 'x,Y,a,b,c,3,4,5,6', 'an index store lands on the live element';
    is @a.splice(0, 2).join(','), 'x,Y', 'splice at the front answers the removed elements';
    is @a.join(','), 'a,b,c,3,4,5,6', '... and drops them';
    is @a.splice(0, 1, <p q r>).join(','), 'a', 'front splice with a longer replacement';
    is @a.join(','), 'p,q,r,b,c,3,4,5,6', '... puts the replacement first';
    is @a.splice(2, 2, 'M').join(','), 'r,b', 'a splice in the middle';
    is @a.join(','), 'p,q,M,c,3,4,5,6', '... replaces the range';
    @a.splice(1, 0, <i j>);
    is @a.join(','), 'p,i,j,q,M,c,3,4,5,6', 'a pure insertion in the middle';
    @a.shift;
    is @a.splice(*-2).join(','), '5,6', 'splice to the end after a shift';
    is @a.join(','), 'i,j,q,M,c,3,4', '... truncates the live range';
}

{
    my %h;
    %h<k> = [1, 2];
    %h<k>.unshift(8, 9);
    %h<k>.prepend(<p q>);
    %h<k>.shift;
    %h<k>.unshift(0);
    is %h<k>.join(','), '0,q,8,9,1,2', 'unshift/prepend on a hash element';

    my $s = [1, 2, 3];
    $s.shift;
    $s.unshift(7, 8);
    $s.prepend(5, 6);
    is $s.join(','), '5,6,7,8,2,3', 'unshift/prepend on a scalar-held array';
}

{
    my @h;
    @h[1] = 'b';
    @h[4] = 'e';
    @h.shift;
    is holes(@h), '1001', 'holes after a shift';
    @h.splice(1, 1, 'X', 'Y');
    is holes(@h), '11101', 'holes move with a splice';
    @h.prepend('P', 'Q');
    is holes(@h), '1111101', 'holes move with a multi-element prepend';
    @h.splice(0, 3);
    is holes(@h), '1101', 'holes move with a front splice';
    is @h.raku, '["X", "Y", Any, "e"]', 'the surviving holes read as Any';
}

{
    my @t = ^10;
    @t.shift for ^3;
    @t[9] = 'z';
    is @t.raku, '[3, 4, 5, 6, 7, 8, 9, Any, Any, "z"]',
        'an autovivifying store past the end after shifts';
    my @q = ^5;
    for ^20 { @q.push($_); @q.shift }
    is @q.join(','), '15,16,17,18,19', 'a push+shift queue';
}

# Timed at two sizes in one process, so machine speed and CI load cancel out.
sub timed(&body --> Num) { my $t0 = now; body(); (now - $t0).Num }

{
    # A fixed number of queue steps on an N-element array: O(1) per step
    # gives ~1x for a 4x larger N; compacting on every push gave ~4x.
    sub queue(int $n) {
        my @q = ^$n;
        timed { for ^20000 { @q.push($_); @q.shift } }
    }
    queue(1000);
    my $small = queue(20000);
    my $large = queue(80000);
    cmp-ok $large / $small, '<', 3, 'queue push+shift does not scale with the array size';
}

{
    # One prepend/splice of N elements onto N elements: linear is ~4x for
    # 4x N, the old per-element insert was ~16x.
    sub prepend(int $n) {
        my @a = ^$n;
        my @b = ^$n;
        timed { @a.prepend(@b) }
    }
    prepend(1000);
    cmp-ok prepend(80000) / prepend(20000), '<', 9, 'prepend of many elements is linear';

    sub splice-front(int $n) {
        my @a = ^$n;
        timed { @a.splice(0, 1) while @a }
    }
    splice-front(1000);
    cmp-ok splice-front(80000) / splice-front(20000), '<', 9, 'a splice(0, 1) loop is linear';
}
