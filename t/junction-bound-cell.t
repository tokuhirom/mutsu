use Test;

# Junction index-assignment through a `:=`-bound container must reach the
# shared cell (regression: it overwrote/detached the bind, dropping writes).

plan 10;

# --- array bind ---
{
    my @b = (10, 20, 30);
    my @a := @b;
    @a[0|1] = 99;
    is @b.gist, '[99 99 30]', 'array junction write propagates to bind source';
    is @a.gist, '[99 99 30]', 'array junction write visible through bound alias';
}

# --- hash bind ---
{
    my %g = (x => 1, y => 2);
    my %h := %g;
    %h{'x'|'y'} = 7;
    is %g.sort.gist, '(x => 7 y => 7)', 'hash junction write propagates to bind source';
    is %h.sort.gist, '(x => 7 y => 7)', 'hash junction write visible through bound alias';
}

# --- chained array bind ---
{
    my @c = (1, 2, 3);
    my @b := @c;
    my @a := @b;
    @a[1|2] = 0;
    is @c.gist, '[1 0 0]', 'array junction write reaches chained bind root';
}

# --- plain (non-bound) cases must stay correct ---
{
    my @a = (1, 2, 3);
    @a[0|2] = 5;
    is @a.gist, '[5 2 5]', 'plain array junction write';

    my %h = (a => 1, b => 2, c => 3);
    %h{'a'|'c'} = 9;
    is %h.sort.gist, '(a => 9 b => 2 c => 9)', 'plain hash junction write';

    my %n;
    %n{'p'|'q'} = 4;
    is %n.sort.gist, '(p => 4 q => 4)', 'fresh hash autoviv via junction';

    my @z = (1,);
    @z[2|4] = 8;
    is @z.gist, '[1 (Any) 8 (Any) 8]', 'plain array junction extend beyond length';
}

# --- bind survives the junction write (still aliased afterwards) ---
{
    my @b = (1, 2, 3);
    my @a := @b;
    @a[0|1] = 5;
    @b[2] = 7;
    is @a.gist, '[5 5 7]', 'bind still live after junction write (later source write seen)';
}
