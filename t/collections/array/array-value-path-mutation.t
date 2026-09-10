use v6;
use Test;

# Value-path (non-variable-invocant) array mutations must write through the
# shared backing node (container identity §3.2): f().splice / f()."pop"() etc.
# reach the runtime slow path, which used to mutate a copy and lose the
# mutation. All expectations raku-verified.

plan 27;

# --- splice via a function result ---
{
    my @a = 1,2,3;
    sub f() { @a }
    my $removed = f().splice(0,1);
    is-deeply $removed, [1], 'f().splice returns the removed elements';
    is-deeply @a, [2,3], 'f().splice mutates the underlying array';
}

{
    my @b = 1,2,3,4;
    sub g() { @b }
    my $removed = g().splice(1,2);
    is-deeply $removed, [2,3], 'splice(1,2) returns two removed elements';
    is-deeply @b, [1,4], 'splice(1,2) removes from the underlying array';
}

{
    my @c = 1,2,3;
    sub h() { @c }
    h().splice(1,1,<x y>);
    is-deeply @c, [1,'x','y',3], 'splice with replacement mutates in place';
}

{
    my @s = 1,2,3;
    sub fs() { @s }
    fs().splice(1,0,fs());
    is-deeply @s, [1,1,2,3,2,3], 'self-splice snapshots the replacement before draining';
}

{
    my @n = 1,2,3;
    sub fn2() { @n }
    my $rem = fn2().splice;
    is-deeply $rem, [1,2,3], 'splice with no args returns everything';
    is @n.elems, 0, 'splice with no args clears the array';
}

{
    my @w = 1,2,3;
    sub fw() { @w }
    fw().splice(*,0,9);
    is-deeply @w, [1,2,3,9], 'splice with Whatever start appends';
}

# --- typed array: splice replacement type check before mutating ---
{
    my Int @t = 1,2,3;
    sub ft() { @t }
    my $died = False;
    try { ft().splice(1,0,"x"); CATCH { default { $died = True } } }
    ok $died, 'splice of a Str into Int array dies';
    is-deeply @t.List, (1,2,3), 'typed array unchanged after failed splice';
}

# --- pop/shift via an indirect method name (slow dispatch path) ---
{
    my @a = 1,2,3;
    sub f2() { @a }
    my $m = "pop";
    my $r = f2()."$m"();
    is $r, 3, 'indirect ."pop"() returns the removed element';
    is-deeply @a, [1,2], 'indirect ."pop"() mutates the underlying array';
}

{
    my @a = 1,2,3;
    sub f3() { @a }
    my $m = "shift";
    my $r = f3()."$m"();
    is $r, 1, 'indirect ."shift"() returns the removed element';
    is-deeply @a, [2,3], 'indirect ."shift"() mutates the underlying array';
}

{
    my @z;
    sub fz() { @z }
    my $m = "pop";
    my $v = fz()."$m"();
    isa-ok $v, Failure, 'indirect pop on empty array returns a Failure';
    is @z.elems, 0, 'empty array does not grow on failed pop';
    $v.so;   # defuse the Failure
}

# --- push family via an indirect method name ---
{
    my @u = 3,4;
    sub fu() { @u }
    my $m = "unshift";
    fu()."$m"(1,2);
    is-deeply @u, [1,2,3,4], 'indirect unshift mutates in place preserving order';
}

{
    my @p = 3,4;
    sub fp() { @p }
    my $m = "prepend";
    fp()."$m"((1,2));
    is-deeply @p, [1,2,3,4], 'indirect prepend flattens and mutates in place';
}

{
    my @g = 1,2;
    sub fg() { @g }
    my $m = "push";
    my $r = fg()."$m"(9);
    is-deeply @g, [1,2,9], 'indirect push mutates in place';
    is-deeply $r, [1,2,9], 'indirect push returns the array';
}

{
    my @g = 1,2;
    sub fa() { @g }
    my $m = "append";
    fa()."$m"((3,4));
    is-deeply @g, [1,2,3,4], 'indirect append flattens and mutates in place';
}

# --- literal invocants still behave (no observable holder) ---
{
    is [1,2,3].splice(0,1), [1], 'literal splice returns removed elements';
    my $m = "pop";
    is [1,2,3]."$m"(), 3, 'literal indirect pop returns the last element';
}

# --- map over inner arrays (slow-path per-element dispatch) ---
{
    my @d = ([1,2,3],[4,5,6]);
    @d.map(*.splice(0,1));
    is-deeply @d, [[2,3],[5,6]], 'map(*.splice) mutates each inner array';
}

{
    my @e = ([1,2],[3,4]);
    my @popped = @e.map(*.pop);
    is-deeply @popped, [2,4], 'map(*.pop) returns popped elements';
    is-deeply @e, [[1,],[3,]], 'map(*.pop) mutates the inner arrays';
}

done-testing;
