use Test;

# An absurd declared shape (`my @a[1e15]`) or buffer size (`Buf.allocate(1e15)`)
# must throw a catchable exception instead of aborting the process via an
# unrecoverable allocation failure (`handle_alloc_error`). This completes the
# autoviv/string-repeat allocation guard (see t/autoviv-index-guard.t): every
# user-controlled-size allocation now reserves fallibly before allocating.
# raku itself aborts here with a MoarVM panic.

plan 17;

# Shaped 1D declaration with an absurd dimension.
{
    my $caught;
    try { my @a[99999999999999]; CATCH { default { $caught = .message } } }
    ok $caught.defined, 'absurd shaped 1D declaration is catchable';
    like $caught, /allocation/, 'shaped error mentions allocation failure';
}

# The process survives (continues past the guarded declaration).
{
    my $survived = False;
    try { my @a[99999999999999]; }
    $survived = True;
    ok $survived, 'process survives an absurd shaped declaration';
}

# Multidimensional shaped declaration with an absurd inner dimension.
{
    my $survived = False;
    try { my @a[3; 99999999999999]; }
    $survived = True;
    ok $survived, 'process survives an absurd multidim shaped declaration';
}

# Array.new(:shape(...)) construction path.
{
    my $caught;
    try { my $a = Array.new(:shape(99999999999999)); CATCH { default { $caught = .message } } }
    ok $caught.defined, 'Array.new(:shape) with an absurd dim is catchable';
}

# Buf.allocate with an absurd size.
{
    my $caught;
    try { my $b = Buf.allocate(99999999999999); CATCH { default { $caught = .message } } }
    ok $caught.defined, 'Buf.allocate with an absurd size is catchable';
    like $caught, /allocation/, 'buffer error mentions allocation failure';
}

# Buf.allocate with an absurd size and a fill argument (a different branch).
{
    my $survived = False;
    try { my $b = Buf.allocate(99999999999999, 0xFF); }
    $survived = True;
    ok $survived, 'process survives Buf.allocate with fill and absurd size';
}

# Normal shaped declarations still work.
{
    my @a[5];
    @a[0] = 1;
    @a[4] = 9;
    is @a.elems, 5, 'normal shaped 1D declaration allocates the slots';
    is @a[4], 9, 'normal shaped 1D declaration stores values';
    is @a.shape.gist, '(5)', 'normal shaped 1D declaration keeps its shape';
}

# Normal multidimensional shaped declaration still works.
{
    my @a[2; 3];
    @a[1; 2] = 'x';
    is @a[1; 2], 'x', 'normal multidim shaped declaration works';
    is @a.shape.gist, '(2 3)', 'normal multidim shaped declaration keeps its shape';
}

# Normal Buf.allocate still works.
{
    my $b = Buf.allocate(4, 7);
    is $b.elems, 4, 'normal Buf.allocate sizes the buffer';
}

# Buf.reallocate to an absurd size is also guarded.
{
    my $b = Buf.allocate(4, 7);
    my $survived = False;
    try { $b.reallocate(99999999999999) }
    $survived = True;
    ok $survived, 'process survives Buf.reallocate to an absurd size';
}

# Normal Buf.reallocate (grow then shrink) still works.
{
    my $b = Buf.allocate(4, 7);
    $b.reallocate(6);
    is $b.elems, 6, 'Buf.reallocate grows the buffer';
    $b.reallocate(2);
    is $b.elems, 2, 'Buf.reallocate shrinks the buffer';
}
