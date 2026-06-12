use Test;

plan 13;

# A `%h is BagHash/MixHash/SetHash` variable IS that QuantHash: element
# assignment sets a weight (not a typed-hash element), assigning 0 removes the
# key, and assigning a non-numeric value throws X::Str::Numeric.
# (roast S02-types/baghash.t, mixhash.t)

# --- MixHash via `is` trait ---
{
    my %h is MixHash = a => 1, b => 0, c => 2;
    lives-ok { %h<c> = 0 }, 'MixHash: can set a weight to 0';
    nok %h<c>:exists, 'MixHash: weight-0 key is gone';
    is %h.elems, 1, 'MixHash: one item left';
    is %h.keys, ('a'), 'MixHash: the right key remains';
    throws-like { %h<a> = "foo" }, X::Str::Numeric,
        'MixHash: assigning a Str weight throws X::Str::Numeric';
}

# --- BagHash via `is` trait ---
{
    my %b is BagHash = a => 1, b => 2;
    %b<a> = 5;
    is %b<a>, 5, 'BagHash: can set a weight';
    %b<a> = 0;
    nok %b<a>:exists, 'BagHash: weight-0 key is gone';
    throws-like { %b<b> = "foo" }, X::Str::Numeric,
        'BagHash: assigning a Str weight throws X::Str::Numeric';
}

# --- BagHash via scalar ---
{
    my $b = <a>.BagHash;
    $b<a> = 42;
    is $b<a>, 42, 'scalar BagHash: can set a weight';
    throws-like { $b<a> = "foo" }, X::Str::Numeric,
        'scalar BagHash: assigning a Str weight throws X::Str::Numeric';
}

# --- MixHash via scalar (already worked; regression guard) ---
{
    my $m = <a>.MixHash;
    $m<a> = 1.5;
    is $m<a>, 1.5, 'scalar MixHash: can set a Real weight';
    throws-like { $m<a> = "foo" }, X::Str::Numeric,
        'scalar MixHash: assigning a Str weight throws X::Str::Numeric';
}

# A real typed hash (value type) is still enforced.
{
    my Int %h;
    throws-like { %h<a> = "x" }, X::TypeCheck::Assignment,
        'typed hash element type is still enforced';
}
