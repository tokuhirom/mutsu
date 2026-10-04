use Test;

plan 3;

# An escaping inner closure reads a variable owned two frames up. The for
# body's own closure is called immediately, but its non-escaping status must
# not stop the captured binding from reaching the owner. The owner writes it
# after the inner closures are created.
sub readers($value) {
    my $shared = 0;
    my @callbacks;
    for 1..3 { @callbacks.push({ $shared }) }
    $shared = $value;
    @callbacks;
}

my @first = readers(42);
sub invoke-with-shadow(&callback) {
    my $shared = 99;
    my $keep = { $shared }; # keep the unrelated caller binding in its env
    callback();
}
is invoke-with-shadow(@first[0]), 42,
    'an inner escaping closure reads its mutated owner, not a caller shadow';
is @first.map({ .() }).join(','), '42,42,42',
    'sibling closures share the owner binding after its write';

my @second = readers(7);
is invoke-with-shadow(@second[0]), 7,
    'a later invocation has its own captured binding';
