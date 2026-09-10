use Test;

# Slice 2a (docs/scalar-array-sharing.md): `$scalar = @arr` shares the source
# Array by reference (raku semantics), not a snapshotting copy.

plan 33;

# --- whole-container structural mutation propagates both ways ---
{
    my @z = (1, 2);
    my $n = @z;
    @z.push(9);
    is-deeply $n.Array, [1, 2, 9], '$n follows @z.push';
    $n.push(8);
    is-deeply @z.Array, [1, 2, 9, 8], '@z follows $n.push';
}

# --- element write propagates (was already working) ---
{
    my @z = (1, 2, 3);
    my $n = @z;
    $n[0] = 100;
    is-deeply @z.Array, [100, 2, 3], '$n[0]= propagates to @z';
    @z[1] = 200;
    is-deeply $n.Array, [100, 200, 3], '@z[1]= propagates to $n';
}

# --- `@copy = @z` stays an independent copy even after @z is shared ---
{
    my @z = (1, 2);
    my $n = @z;               # promotes @z to a shared cell
    my @copy = @z;            # must still be a copy
    @z.push(7);
    is-deeply @copy.Array, [1, 2], '@copy = @z is independent of @z.push';
    is-deeply $n.Array, [1, 2, 7], '$n still follows @z';
    @copy.push(99);
    is-deeply @z.Array, [1, 2, 7], '@copy.push does not leak to @z';
}

# --- scalar reassign to a non-array REPLACES the slot (drops share) ---
{
    my @z = (1, 2);
    my $n = @z;
    $n = 5;
    is $n, 5, '$n = 5 replaces the slot';
    @z.push(9);
    is-deeply @z.Array, [1, 2, 9], '@z still usable after $n reassigned';
    is $n, 5, '$n stays 5 (no longer shares @z)';
}

# --- array var whole-reassign mutates through, visible to scalar ---
{
    my @z = (1, 2);
    my $n = @z;
    @z = (9, 9, 9);
    is-deeply $n.Array, [9, 9, 9], '@z = (...) visible via $n';
}

# --- reassign scalar to ANOTHER array re-shares with that one ---
{
    my @a = (1);
    my @b = (2);
    my $p = @a;
    $p = @b;
    @b.push(99);
    is-deeply $p.Array, [2, 99], '$p = @b re-shares with @b';
    @a.push(88);
    is-deeply $p.Array, [2, 99], '$p no longer follows @a';
}

# --- hash sharing ---
{
    my %h = (a => 1);
    my $hr = %h;
    %h<b> = 2;
    is $hr<b>, 2, '$hr follows %h<b>=';
    $hr<c> = 3;
    is %h<c>, 3, '%h follows $hr<c>=';
}

# --- reassign in a fresh scope does not corrupt outer ---
{
    my @z = (1, 2);
    my $n = @z;
    {
        my $m = @z;
        $m.push(3);
    }
    is-deeply @z.Array, [1, 2, 3], 'inner-scope $m.push propagates';
    is-deeply $n.Array, [1, 2, 3], '$n sees inner-scope push';
}

# --- unshift / pop through the shared scalar ---
{
    my @z = (2, 3);
    my $n = @z;
    @z.unshift(1);
    is-deeply $n.Array, [1, 2, 3], '$n follows @z.unshift';
    $n.pop;
    is-deeply @z.Array, [1, 2], '@z follows $n.pop';
}

# --- copy semantics for plain `@copy = @z` (no share involved) still hold ---
{
    my @z = (1, 2);
    my @copy = @z;
    @z.push(3);
    is-deeply @copy.Array, [1, 2], 'plain @copy = @z is a copy';
}

# --- shared scalar inside a say/stringification is itemized ---
{
    my @z = (1, 2);
    my $n = @z;
    is $n.elems, 2, '$n.elems works on shared array';
    is $n.WHAT.gist, '(Array)', '$n.WHAT is Array';
}

# --- multiple scalars share the same source ---
{
    my @z = (1);
    my $a = @z;
    my $b = @z;
    @z.push(2);
    is-deeply $a.Array, [1, 2], 'first scalar shares';
    is-deeply $b.Array, [1, 2], 'second scalar shares';
}

# --- Slice 2b: chained scalar share `$r = $q` (scalar RHS holding an array) ---
{
    my @y = (1, 2);
    my $q = @y;
    my $r = $q;
    @y.push(7);
    is-deeply $r.Array, [1, 2, 7], 'chained $r = $q follows @y';
    $r.push(8);
    is-deeply @y.Array, [1, 2, 7, 8], '@y follows chained $r.push';
}

# --- chained share from a scalar holding an anonymous array ---
{
    my $a = [1, 2, 3];
    my $b = $a;
    $a.push(4);
    is-deeply $b.Array, [1, 2, 3, 4], '$b = $a (anon array) shares';
}

# --- plain scalar-to-scalar assignment stays a copy (no spurious share) ---
{
    my $x = 5;
    my $y = $x;
    $x = 9;
    is $y, 5, 'plain $y = $x is a copy';
}

# --- chained scalar reassigned to non-array drops the share ---
{
    my @z = (1, 2);
    my $p = @z;
    my $q = $p;
    $q = 99;
    is-deeply @z.Array, [1, 2], 'chained $q reassign does not touch @z';
    is-deeply $p.Array, [1, 2], '$p still intact';
    is $q, 99, '$q holds the new scalar';
}

# --- a `:=`-bound scalar holding a non-array, chained `=`, stays a copy ---
{
    my $b = 5;
    my $a := $b;
    my $c = $a;
    $b = 9;
    is $c, 5, 'chained = from a bound Int scalar copies the value';
}

# --- chained hash share ---
{
    my %h = (x => 1);
    my $hr = %h;
    my $hr2 = $hr;
    %h<y> = 2;
    is $hr2<y>, 2, 'chained hash share follows %h';
}
