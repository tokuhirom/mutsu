use Test;

# `.=` metaop on an `is rw` accessor writes the result back through the
# attribute slot (`$obj.attr .= meth` is `$obj.attr = $obj.attr.meth`),
# rather than snapshotting the accessor's value into a throwaway temporary.

plan 12;

class Counter {
    has $.n is rw;
}

{
    my $c = Counter.new(n => 1);
    $c.n .= succ;
    is $c.n, 2, 'scalar accessor .= succ writes back';
}

{
    my $c = Counter.new(n => 1);
    ($c.n).=succ;
    is $c.n, 2, 'grouped accessor (.= ) writes back';
}

{
    my $c = Counter.new(n => 'a');
    $c.n .= uc;
    is $c.n, 'A', 'string accessor .= uc writes back';
}

{
    my $c = Counter.new(n => 5);
    my $r = ($c.n .= succ);
    is $r, 6, '.= returns the new value';
    is $c.n, 6, '... and the attribute is updated';
}

class Holder {
    has @.items is rw;
}

{
    my $h = Holder.new(items => [3, 1, 2]);
    $h.items .= sort;
    is-deeply $h.items, [1, 2, 3], 'array accessor .= sort writes back';
}

# `.=` with method arguments on an accessor.
class Box {
    has $.s is rw;
}
{
    my $b = Box.new(s => 'hello world');
    $b.s .= subst('world', 'raku');
    is $b.s, 'hello raku', 'accessor .= subst(args) writes back';
}

# A Proxy-returning `is rw` accessor still routes through STORE rather than the
# attribute-slot writeback path.
my @stored;
class Proxied {
    method val is rw {
        Proxy.new(
            FETCH => method () { 10 },
            STORE => method ($v) { @stored.push($v) },
        )
    }
}
{
    my $p = Proxied.new;
    $p.val .= succ;
    is-deeply @stored, [11], 'Proxy accessor .= succ routes through STORE';
}

# Plain variable `.=` is unchanged.
{
    my $x = 41;
    $x .= succ;
    is $x, 42, 'plain scalar variable .= succ still works';
}

# Array element `.=` is unchanged.
{
    my @a = 1, 2, 3;
    @a[0] .= succ;
    is-deeply @a, [2, 2, 3], 'array element .= succ still works';
}

# Non-lvalue method call (constructor) `.=` on a typed declaration still works.
{
    my Counter $c .= new(n => 7);
    is $c.n, 7, 'typed-declaration .= new still works';
}

# Chained: accessor whose value is itself an object, `.=` on a nested accessor.
class Outer {
    has Counter $.inner is rw;
}
{
    my $o = Outer.new(inner => Counter.new(n => 1));
    $o.inner.n .= succ;
    is $o.inner.n, 2, 'nested accessor .= succ writes back';
}

# vim: expandtab shiftwidth=4
