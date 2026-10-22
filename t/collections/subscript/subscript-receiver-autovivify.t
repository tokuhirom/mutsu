use Test;

# A vivifying method (`push`, `ASSIGN-KEY`, `BIND-KEY`, ...) called on a
# subscript element that is undefined stores the fresh container through the
# element's location, for any base expression (#13823). Expected values are
# the `raku` output.

plan 22;

{
    my @a;
    @a[1].ASSIGN-KEY("e", 9);
    is @a.raku, '[Any, {:e(9)}]', 'ASSIGN-KEY vivifies a missing array element to a Hash';
}
{
    my %h;
    %h<k>.ASSIGN-KEY("e", 1);
    is %h.raku, '{:k(${:e(1)})}', 'ASSIGN-KEY vivifies a missing hash entry';
}
{
    my @d;
    @d[0].BIND-KEY("z", 3);
    is @d.raku, '[{:z(3)},]', 'BIND-KEY vivifies a missing array element to a Hash';
}
{
    class K { has $.attr is rw }
    my $k = K.new;
    $k.attr.push(1);
    is $k.attr.raku, '$[1]', 'push on an rw attribute vivifies the attribute';
    $k.attr.push(2);
    is $k.attr.raku, '$[1, 2]', 'and a second push grows it';
}
{
    sub f(@x) { @x }
    my @c;
    f(@c)[2].push(5);
    is @c.raku, '[Any, Any, [5]]', 'push on an element of a call result';
}
{
    my @e;
    my $m = "push";
    @e[1]."$m"(4);
    is @e.raku, '[Any, [4]]', 'a dynamic method name vivifies too';
}
{
    class P { method push(|) { "P.push" } }
    my @q;
    @q[0] = P;
    is @q[0].push(1), 'P.push', 'a type-object element reaches its own push';
    is @q[0].^name, 'P', 'and is not replaced by an Array';
}
{
    my @t;
    @t[3].push(1);
    is @t.raku, '[Any, Any, Any, [1]]', 'push on a hole past the end';
    my %g;
    %g<a>.push(1);
    is %g.raku, '{:a($[1])}', 'push on a missing hash entry';
    %g<a>.push(2);
    is %g<a>.raku, '$[1, 2]', 'a second push grows the stored Array';
}
{
    my @i = 1, 2;
    @i[0].push(3) if False;
    my @j;
    @j[0] = [1];
    @j[0].push(2);
    is @j.raku, '[[1, 2],]', 'an existing Array element grows in place';
}
{
    my Int @z;
    dies-ok { @z[0].push(1) }, 'a typed array element refuses the Array';
    my Array of Int @x;
    @x[0].push(3);
    is @x[0].raku, 'Array[Int].new(3)', 'a typed hole vivifies a typed Array';
    dies-ok { @x[0].push('foo') }, 'and enforces its element type';
}
{
    my %h;
    %h{any 0, 1}.push(42);
    is %h.keys.sort.join(','), '0,1', 'a junction subscript vivifies every key';
}
{
    my @b is default(7);
    dies-ok { @b[2].push(1) }, 'an element holding the default is defined';
}
{
    sub g { Any }
    throws-like { g().push(1) }, X::Assignment::RO, 'a call result has nowhere to vivify';
    class R { has $.ro }
    my $r = R.new;
    throws-like { $r.ro.push(1) }, X::Assignment::RO, 'a read-only accessor value cannot be vivified';
}
{
    my @n;
    @n[0].append(1, 2);
    @n[1].unshift(3);
    @n[2].prepend(4, 5);
    is @n.raku, '[[1, 2], [3], [4, 5]]', 'append, unshift and prepend vivify';
}
{
    my %w;
    %w<x><y>.push(1);
    is %w.raku, '{:x(${:y($[1])})}', 'a nested subscript still vivifies';
}
