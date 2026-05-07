use Test;

plan 6;

# Test "Did you mean" suggestions for misspelled methods

# Str method misspelling
{
    my $err = '';
    try { "hello".upercase; CATCH { default { $err = .message } } }
    ok $err.contains("Did you mean 'uppercase'?"), "suggests 'uppercase' for 'upercase' on Str";
}

# Array method misspelling
{
    my $err = '';
    try { [1,2,3].pus(4); CATCH { default { $err = .message } } }
    ok $err.contains("Did you mean 'push'?"), "suggests 'push' for 'pus' on Array";
}

# Double-letter typo
{
    my $err = '';
    try { "hello".uupercase; CATCH { default { $err = .message } } }
    ok $err.contains("Did you mean 'uppercase'?"), "suggests 'uppercase' for 'uupercase' on Str";
}

# No suggestion for completely unrelated method
{
    my $err = '';
    try { "hello".xyz; CATCH { default { $err = .message } } }
    ok !$err.contains("Did you mean"), "no suggestion for 'xyz' on Str";
}

# Numeric type misspelling
{
    my $err = '';
    try { 42.squirt; CATCH { default { $err = .message } } }
    ok $err.contains("Did you mean 'sqrt'?"), "suggests 'sqrt' for 'squirt' on Int";
}

# Hash method misspelling
{
    my $err = '';
    try { my %h = a => 1; %h.keypairs; CATCH { default { $err = .message } } }
    ok $err.contains("Did you mean"), "suggests something for 'keypairs' on Hash";
}
