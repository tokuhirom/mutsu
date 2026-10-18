use Test;

# `Any`'s growing methods autovivify an undefined binding to an `Array` from
# their row (ADR-11276 §9.171), wherever the binding lives, and answer the
# `Array` itself. Expected values are Rakudo's.

plan 16;

{
    my $x;
    is $x.push(1, 2).raku, '[1, 2]', 'push on an undefined $ answers the Array';
    is $x.raku, '$[1, 2]', '... which the Scalar now holds';
}
{
    my $m = 'append';
    my $y;
    $y."$m"(3, 4);
    is $y.raku, '$[3, 4]', 'a run-time method name';
}
{
    my @a;
    is @a[2].push(5).raku, '[5]', 'an array element';
    is @a.raku, '[Any, Any, [5]]', '... lands in the array';
}
{
    my Array $t;
    $t.push(7);
    is $t.raku, '$[7]', 'an Array-typed scalar';
}
{
    our $o;
    $o.unshift(1);
    is $o.raku, '$[1]', 'an our variable';
    @GLOBAL::g.push(1);
    is @GLOBAL::g.raku, '$[1]', 'a never-written package-qualified slot';
}
{
    my %h;
    %h<k>.push(9);
    is %h.raku, '{:k($[9])}', 'a hash value';
}
{
    sub f($p is rw) { $p.prepend(1, 2) }
    my $z;
    f($z);
    is $z.raku, '$[1, 2]', 'an is rw parameter';
}
{
    my @rows = Any, Any;
    for @rows <-> $r { $r.push(1) }
    is @rows.raku, '[[1], [1]]', 'a <-> loop parameter';
}
{
    my $c := my $d;
    $c.push(8);
    is $d.raku, '$[8]', 'a := alias';
}
{
    my $q;
    is $q.+push(1).raku, '([1],)', '.+push vivifies once';
}

# The Array row answers the Array, not the item a $ binding holds.
{
    my $y = [1];
    is $y.push(2).raku, '[1, 2]', 'push on an itemized array';
    is $y.unshift(0).raku, '[0, 1, 2]', 'unshift on an itemized array';
    is $y.raku, '$[0, 1, 2]', '... which stays itemized in its Scalar';
}
