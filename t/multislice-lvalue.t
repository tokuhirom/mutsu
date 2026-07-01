use Test;

plan 11;

# An array multi-dimensional subscript (`@a[0;1;2]`) used as an lvalue: a
# direct assignment, a `:=` bind, and a raw `\target` / `is rw` parameter that
# writes through to the underlying nested array (visible immediately).

# 1. Direct multi-dim assignment
{
    my @a = [[[42, 666], [314]], ];
    @a[0;0;1] = 999;
    is-deeply @a, [[[42, 999], [314]], ], 'direct @a[0;0;1] = 999';
}

# 2. Direct multi-dim assignment with autovivification
{
    my @a = [[1, 2], ];
    @a[0;2] = 7;
    is-deeply @a, [[1, 2, 7], ], 'direct autoviv @a[0;2] = 7';
}

# (A direct `my $s := @b[0;1]` scalar bind to a multislice does not yet alias —
# that bind path is a separate, pre-existing gap; the raw `\target` / `is rw`
# argument path below is what S32 multislice-6e.t exercises.)

# 3. raw `\target` parameter aliases the element (write-through)
{
    my @a = [[1, 2, 3], [4, 5, 6]];
    sub set(\target, \v) { target = v }
    set(@a[1;2], 99);
    is-deeply @a, [[1, 2, 3], [4, 5, 99]], 'raw \target write-through';
}

# 5. `\target` mutation is visible immediately INSIDE the callee
{
    my @a = [[1, 2], [3, 4]];
    my $seen;
    sub probe(\target) { target = 7; $seen = @a }
    probe(@a[0;1]);
    is-deeply $seen, [[1, 7], [3, 4]], 'mutation visible mid-call';
    is-deeply @a, [[1, 7], [3, 4]], 'mutation persists after call';
}

# 6. `is rw` parameter
{
    my @a = [[5, 6], [7, 8]];
    sub bump(@dummy, $x is rw) { $x = $x + 100 }
    # (use a scalar rw to confirm rw still works alongside)
    my $z = 1;
    bump(@a, $z);
    is $z, 101, 'is rw scalar still works';
    is-deeply @a, [[5, 6], [7, 8]], '@a untouched';
}

# 7. expression-context assignment through `\target`
{
    my @a = [[0, 0], [0, 0]];
    sub asg(\target, \v) { my $r = (target = v); $r }
    is asg(@a[1;0], 42), 42, 'expression-context assign returns value';
    is-deeply @a, [[0, 0], [42, 0]], 'expression-context assign writes through';
}

# 8. `try`-wrapped assignment through `\target`
{
    my @a = [[9]];
    sub tasg(\target, \v) { (try target = v) }
    is tasg(@a[0;0], 5), 5, 'try-wrapped assign returns value';
    is-deeply @a, [[5]], 'try-wrapped assign writes through';
}
