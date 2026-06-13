use Test;

# Whole-container `:=` bind (`my @b := @a` / `my %g := %h`) makes the two
# variables share a single container, so mutations through either alias are
# observed by both (Track B element-cell: array-backed mutation). Before the
# fix, the two slots only shared the inner Arc and a COW mutation (`.push`)
# detached them.

plan 33;

# --- Array: push through either alias propagates both ways ---
{
    my @a = 1, 2, 3;
    my @b := @a;
    @b.push(4);
    is-deeply @a, [1, 2, 3, 4], 'push via bound alias propagates to source';
    @a.push(5);
    is-deeply @b, [1, 2, 3, 4, 5], 'push via source propagates to bound alias';
}

# --- Array: single + slice + nested index assignment ---
{
    my @a = 1, 2, 3;
    my @x := @a;
    @x[0] = 99;
    is-deeply @a, [99, 2, 3], 'single index assign via alias';
    @a[1] = 88;
    is-deeply @x, [99, 88, 3], 'single index assign via source';
    @x[1, 2] = 10, 20;
    is-deeply @a, [99, 10, 20], 'slice assign via alias';
}
{
    my @a = [1, 2], [3, 4];
    my @x := @a;
    @x[0][1] = 99;
    is-deeply @a, [[1, 99], [3, 4]], 'nested index assign via alias';
}

# --- Array: pop / shift / unshift / splice through alias ---
{
    my @a = 1, 2, 3, 4, 5;
    my @b := @a;
    @a.pop;
    is-deeply @b, [1, 2, 3, 4], 'pop via source seen by alias';
    @a.shift;
    is-deeply @b, [2, 3, 4], 'shift via source seen by alias';
    @a.unshift(0);
    is-deeply @b, [0, 2, 3, 4], 'unshift via source seen by alias';
    @a.splice(1, 1);
    is-deeply @b, [0, 3, 4], 'splice via source seen by alias';
    @b.push(99);
    is-deeply @a, [0, 3, 4, 99], 'push via alias seen by source';
}

# --- Array: delete through alias ---
{
    my @a = 1, 2, 3, 4;
    my @b := @a;
    @a[1]:delete;
    is-deeply @b, [1, Any, 3, 4], 'delete via source seen by alias';
}

# --- Array: bind then plain reassign source ---
{
    my @a = 1, 2, 3;
    my @b := @a;
    @a = (4, 5, 6);
    is-deeply @b, [4, 5, 6], 'plain reassign of source seen by alias';
}

# --- Array: rebind source detaches the old alias ---
{
    my @a = 1, 2, 3;
    my @b := @a;
    my @c = 7, 8, 9;
    @a := @c;
    @a.push(10);
    is-deeply @b, [1, 2, 3], 'rebound source no longer affects old alias';
    is-deeply @a, [7, 8, 9, 10], 'rebound source tracks new container';
}

# --- Array: three-way chained alias shares one container ---
{
    my @a = 1, 2;
    my @b := @a;
    my @c := @b;
    @c.push(3);
    is-deeply @a, [1, 2, 3], 'chained alias: source sees push';
    is-deeply @b, [1, 2, 3], 'chained alias: middle sees push';
}

# --- Array: cross-frame parameter alias ---
{
    sub mutate(@arr) {
        my @local := @arr;
        @local.push(99);
    }
    my @a = 1, 2, 3;
    mutate(@a);
    is-deeply @a, [1, 2, 3, 99], 'bound alias inside sub mutates caller array';
}

# --- Array: bind to a literal (no source variable) still mutable ---
{
    my @b := [1, 2, 3];
    @b.push(4);
    is-deeply @b, [1, 2, 3, 4], 'bind to array literal is mutable';
}

# --- Hash: bidirectional element assign + delete through alias ---
{
    my %h = a => 1, b => 2;
    my %g := %h;
    %g<c> = 3;
    is %h<c>, 3, 'hash element assign via alias propagates to source';
    %h<d> = 4;
    is %g<d>, 4, 'hash element assign via source propagates to alias';
    %g<b>:delete;
    is-deeply %h.keys.sort, ('a', 'c', 'd'), 'hash delete via alias seen by source';
}

# --- Iteration / read methods over a bound alias decontainerize ---
{
    my @a = 1, 2, 3;
    my @b := @a;
    @a.push(4);
    is ([+] @b), 10, 'reduce over bound alias sees latest';
    is @b.elems, 4, 'elems over bound alias';
    is @b[2], 3, 'index read over bound alias';
}

# --- A bound container adopts the source's declared element type ---
{
    my Int %a;
    my Cool %b := %a;
    is %b.of, Int, 'bound hash keeps the source declared element type';
}

# --- Scalar bound to a whole container (`my $r := @a`) shares it too ---
{
    my @a = 1, 2, 3;
    my $r := @a;
    $r.push(4);
    is-deeply @a, [1, 2, 3, 4], 'push via scalar-bound array propagates to source';
    @a.push(5);
    is-deeply $r, [1, 2, 3, 4, 5], 'push via source propagates to scalar alias';
    $r[0] = 99;
    is-deeply @a, [99, 2, 3, 4, 5], 'index assign via scalar alias';
    $r.pop;
    is-deeply @a, [99, 2, 3, 4], 'pop via scalar alias propagates to source';
    $r.splice(1, 1);
    is-deeply @a, [99, 3, 4], 'splice via scalar alias propagates to source';
}
{
    my %h = a => 1;
    my $hr := %h;
    $hr<b> = 2;
    is %h<b>, 2, 'hash element assign via scalar alias propagates to source';
    $hr<a>:delete;
    is-deeply %h.keys.sort, ('b',), 'hash delete via scalar alias seen by source';
}
