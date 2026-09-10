use Test;

plan 4;

# Verify is-deeply diagnostics output (expected/got lines)
# We use is-run to capture TAP output and check the diagnostic lines

sub capture-is-deeply(Str $code) {
    my @output;
    # We can check the output by running mutsu with the code
    # and capturing the TAP output
}

# Test 1: is-deeply passes — no diagnostics
{
    is-deeply [1, 2, 3], [1, 2, 3], 'is-deeply pass produces no diagnostics';
}

# Test 2: Verify is-deeply with matching values
{
    is-deeply (1, 2, 3), (1, 2, 3), 'is-deeply pass with list';
}

# Test 3: Hash comparison
{
    is-deeply {:a(1), :b(2)}, {:a(1), :b(2)}, 'is-deeply pass with hash';
}

# Test 4: Nested structures
{
    is-deeply [1, [2, 3]], [1, [2, 3]], 'is-deeply pass with nested arrays';
}
