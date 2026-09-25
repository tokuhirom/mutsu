use Test;

plan 2;

# Env::File exposed this when a missing file made a declaration initializer
# fail inside a loop: the next declaration saw the previous iteration's value.
my @values;
for <fail success fail> -> $mode {
    try my $value = $mode eq 'success' ?? 'ok' !! die 'initializer failed';
    @values.push($value.raku);
}

is @values[0], 'Any', 'a failed declaration initializer leaves Any';
is @values[2], 'Any', 'a later failed initializer does not reuse the prior value';
