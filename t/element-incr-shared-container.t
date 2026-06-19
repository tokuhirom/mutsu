use Test;

# Post/pre increment and decrement on an *element* of a container that is shared
# through a `ContainerRef` cell (a scalar-bound container param, `my $s = @a`,
# Slice 2a/2d) must mutate the caller's container, like `$n[0] = …` and
# `$n[0] += …` already did. Two representations occur:
#   * a raw `ContainerRef` cell (positional scalar-bound param, `:=` deref bind)
#     -> handled by incrementing through the cell;
#   * a deref'd-but-Arc-shared plain container (named param, `my $s = @a`)
#     -> handled by the strong_count>1 in-place writeback (the array path always
#        did this; the hash path used to `Arc::make_mut`-detach and drop the
#        write).

plan 14;

# --- scalar holding an array (Slice 2a) ---
{
    my @a = (5, 6);
    my $s = @a;
    $s[0]++;
    is @a.gist, '[6 6]', '$s[0]++ through scalar-array share propagates';
}
{
    my @a = (5, 6);
    my $s = @a;
    $s[1]--;
    is @a.gist, '[5 5]', '$s[1]-- through scalar-array share propagates';
}
{
    my @a = (5,);
    my $s = @a;
    ++$s[0];
    is @a.gist, '[6]', 'pre-increment ++$s[0] propagates';
}

# --- scalar holding a hash ---
{
    my %h = (k => 0);
    my $s = %h;
    $s<k>++;
    is %h<k>, 1, '$s<k>++ through scalar-hash share propagates';
}
{
    my %h = (k => 3);
    my $s = %h;
    $s<k>--;
    is %h<k>, 2, '$s<k>-- through scalar-hash share propagates';
}
{
    my %h = (k => "aa");
    my $s = %h;
    $s<k>++;
    is %h<k>, "ab", 'string magical increment through shared hash propagates';
}

# --- positional scalar param holding an array ---
sub pa($n) { $n[0]++ }
{
    my @a = (10, 20);
    pa(@a);
    is @a.gist, '[11 20]', '$n[0]++ through positional $-param array propagates';
}

# --- positional scalar param holding a hash ---
sub ph($h) { $h<k>++ }
{
    my %h = (k => 7);
    ph(%h);
    is %h<k>, 8, '$h<k>++ through positional $-param hash propagates';
}

# --- multiple increments in one call ---
sub multi($n) { $n[0]++; $n[0]++; $n[1]++ }
{
    my @a = (10, 20);
    multi(@a);
    is @a.gist, '[12 21]', 'repeated element increments all propagate';
}

# --- plain (unshared) array/hash element increment still works ---
{
    my @a = (1, 2);
    @a[0]++;
    is @a.gist, '[2 2]', 'plain @a[0]++ still works';
}
{
    my %h = (k => 1);
    %h<k>++;
    is %h<k>, 2, 'plain %h<k>++ still works';
}

# --- `:=`-bound deref scalar (raw ContainerRef cell) ---
{
    my @a = (5,);
    my $s := @a;
    $s[0]++;
    is @a.gist, '[6]', '$s[0]++ through := deref bind propagates';
}

# --- compound assign on a shared element still works (regression guard) ---
{
    my @a = (5,);
    my $s = @a;
    $s[0] += 10;
    is @a.gist, '[15]', '$s[0] += 10 still propagates';
}

# --- increment return value is the pre-increment value (post) ---
{
    my @a = (5,);
    my $s = @a;
    my $r = $s[0]++;
    is $r, 5, 'post-increment returns the original value';
}
