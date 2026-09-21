# The chained element store `@a[$i][$j] = $v` gained a fast lane (#8069), so
# every rule `exec_index_assign_expr_nested_op_body` owns has to stay owned by
# it: the lane must DECLINE for each of these shapes rather than serve them.
#
# Each block below is one decline reason, named in the lane's preconditions.
# All expectations were taken from real rakudo, not from mutsu's prior output.
use Test;
plan 20;

# --- the shapes the lane serves -------------------------------------------

{
    my @g;
    @g[$_] = [0 xx 4] for ^4;
    @g[1][2] = 99;
    @g[0][0] = 7;
    is @g.raku, '[[7, 0, 0, 0], [0, 0, 99, 0], [0, 0, 0, 0], [0, 0, 0, 0]]',
        'array-of-array element store';
}

{
    my %hh;
    %hh<a> = {};
    %hh<a><b> = 5;
    %hh<a><c> = 6;
    is %hh.raku, '{:a(${:b(5), :c(6)})}', 'hash-of-hash element store';
}

{
    my %ha;
    %ha<k> = [1, 2, 3];
    %ha<k>[1] = 9;
    is %ha.raku, '{:k($[1, 9, 3])}', 'hash-of-array element store';
}

{
    my @ah;
    @ah[0] = {};
    @ah[0]<x> = 3;
    is @ah.raku, '[{:x(3)},]', 'array-of-hash element store';
}

{
    # The assignment's own value is the rvalue, itemized like any element store.
    my @g = [1, 2], [3, 4];
    my $r = (@g[0][1] = 42);
    is $r, 42, 'a chained store evaluates to the assigned value';
    is @g.raku, '[[1, 42], [3, 4]]', 'and the store landed';
}

{
    # An Int key on a hash stringifies, exactly as a single-level store does.
    my %h;
    %h<r> = {};
    %h<r>{3} = 'x';
    is %h<r>{3}, 'x', 'a non-Str hash key stringifies at the second level';
}

# --- container identity: the write goes THROUGH the shared node ------------

{
    my @s = [1, 2], [3, 4];
    my @alias := @s;
    @s[0][0] = 77;
    is @alias.raku, '[[77, 2], [3, 4]]',
        'a chained store is observed through a := alias of the root';
}

{
    my @src = [1, 2], [3, 4];
    my @cp = @src;
    @src[0][0] = 55;
    is @cp.raku, '[[55, 2], [3, 4]]',
        'rows are shared by a copy, so the row write reaches it (raku does this too)';
}

{
    # A `:=`-bound row is a shared ContainerRef cell: the store must write
    # through it, reaching the source array.
    my @row = 1, 2, 3;
    my @c;
    @c[0] := @row;
    @c[0][1] = 42;
    is @row.raku, '[1, 42, 3]', 'a := bound row is written through, not replaced';
}

# --- shapes the lane must decline -----------------------------------------

{
    # Autovivification: the lane requires both levels to already exist.
    my @v;
    @v[2][3] = 8;
    is @v.raku, '[Any, Any, [Any, Any, Any, 8]]', 'a missing row still autovivifies';
    my %v2;
    %v2<p><q> = 1;
    is %v2.raku, '{:p(${:q(1)})}', 'a missing hash level still autovivifies';
}

{
    # Out of range at the second level: the row grows, with Any holes.
    my @o = [1, 2], [3, 4];
    @o[0][5] = 9;
    is @o.raku, '[[1, 2, Any, Any, Any, 9], [3, 4]]',
        'an out-of-range second subscript still resizes the row';
}

{
    # A typed root refuses an autovivified container element.
    my Int @t;
    my $died = False;
    { @t[0][1] = 5; CATCH { default { $died = True } } }
    ok $died, 'a typed array still refuses a chained autovivification';
}

{
    # #7556 C2: a bare element of a List/ItemList is immutable. The refusal
    # message is built lazily now (it used to be rendered on EVERY chained
    # store), so this pins its exact wording, not just that it throws.
    my @l = (1, 2), 3;
    my $msg = '';
    { @l[0][0] = 9; CATCH { default { $msg = .message } } }
    is $msg, 'Cannot modify an immutable List ((1 2))',
        'a bare List element is refused, naming the List';
}

{
    # The three-level chain reaches the same refusal through its own site.
    my @b = [ (1, 2), 3 ], 4;
    my $msg = '';
    { @b[0][0][0] = 9; CATCH { default { $msg = .message } } }
    is $msg, 'Cannot modify an immutable List ((1 2))',
        'the 3+ level chain refuses a bare List element the same way';
}

{
    # A slice at the second level distributes; the lane serves single elements.
    my @j;
    @j[$_] = [0 xx 4] for ^2;
    @j[0][0, 1] = 5, 6;
    is @j.raku, '[[5, 6, 0, 0], [0, 0, 0, 0]]', 'a slice second subscript still distributes';
}

{
    # A Whatever-code subscript resolves against the container it indexes.
    my @w = [1, 2, 3], [4, 5, 6];
    @w[0][*-1] = 99;
    is @w.raku, '[[1, 2, 99], [4, 5, 6]]', 'a *-1 second subscript still resolves';
}

# --- two divergences found while writing this file ------------------------
#
# Neither is reachable by the fast lane: `Nil` is absent from its rvalue
# allow-list and a `Proxy` element is on its destination reject-list, so in
# both cases it declines and the unchanged body runs. Both are fixed now
# (#8965, #8966), so both are live pins here; their wider pins are
# t/vm/binding/bind-chained-proxy-store.t and
# t/vm/binding/nil-decay-chained-element-store.t.

{
    # A Proxy element mediates its own store: the chained store fires its
    # STORE rather than overwriting the container (#8965).
    my @p;
    @p[0] = [0];
    my $backing = 0;
    @p[0][0] := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v * 10 });
    @p[0][0] = 7;
    is $backing, 70, 'a Proxy element at the second level still mediates the store';
}

{
    # `is default` decides what a Nil store writes: the store decays it to the
    # default of the container it lands in (ADR-0049). The row is a plain
    # Array, so that default is `Any`, not the root's 42 (#8966). The wider
    # pin is t/vm/binding/nil-decay-chained-element-store.t.
    my @d is default(42);
    @d[0] = [1, 2];
    @d[0][0] = Nil;
    is @d[0][0], Any, 'a Nil store into a row of a defaulted array still takes the full path';
}
