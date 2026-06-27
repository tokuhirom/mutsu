use Test;

# Regression: a multi-dimensional index assignment (`@a[i;j] = v`, `%h{a;b} = v`)
# must write back even when the container was assigned by a sub/closure that
# captured the outer variable. Such an assignment leaves the variable bound to a
# shared `ContainerRef` cell; the multidim-assign path used to mutate only the
# env snapshot and silently drop the write, so the outer variable kept its old
# value. (Simple single-index assignment already handled the shared cell.)

plan 8;

{
    my @a;
    sub set_twod(--> Nil) { @a = [[1, 2], [3, 4]]; }
    set_twod;
    @a[0;1] = 999;
    is-deeply @a, [[1, 999], [3, 4]], 'multidim assign persists after sub-assigned 2D array';
}

{
    my @a;
    sub set_threed(--> Nil) { @a = [[[42, 666, [314]],],]; }
    set_threed;
    @a[0;0;0] = 999;
    is-deeply @a, [[[999, 666, [314]],],], 'multidim assign persists after sub-assigned 3D array';
}

{
    # The whole assignment still evaluates to the assigned value.
    my @a;
    sub setup(--> Nil) { @a = [[1, 2], [3, 4]]; }
    setup;
    my $r = (@a[1;0] = 77);
    is $r, 77, 'multidim assign expression returns the assigned value';
    is-deeply @a, [[1, 2], [77, 4]], 'value landed at the right place';
}

{
    # Hash multidim assignment through a sub-assigned hash.
    my %h;
    sub seth(--> Nil) { %h = (a => {b => 1}); }
    seth;
    %h{"a";"b"} = 99;
    is %h<a><b>, 99, 'hash multidim assign persists after sub-assigned hash';
}

{
    # Closure (block) capture, not just a named sub.
    my @a;
    my $init = -> { @a = [[10, 20], [30, 40]]; };
    $init();
    @a[1;1] = 5;
    is-deeply @a, [[10, 20], [30, 5]], 'multidim assign persists after closure-assigned array';
}

# Plain (non-captured) declarations must keep working unchanged.
{
    my @a = [[1, 2], [3, 4]];
    @a[0;0] = 8;
    is-deeply @a, [[8, 2], [3, 4]], 'multidim assign on a directly-declared array still works';
}
{
    my @a = [1, 2, 3];
    @a[1] = 9;
    is-deeply @a, [1, 9, 3], 'simple index assign unaffected';
}
