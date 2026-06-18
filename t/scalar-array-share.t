use Test;

# Slice 2a (docs/scalar-array-sharing.md): `$scalar = @arr` shares the source
# Array by reference (raku semantics), not a snapshotting copy.

plan 24;

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
