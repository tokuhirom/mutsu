use Test;

plan 8;

# A C-style `loop (...)` spec must have exactly 3 semicolon-separated
# expressions. A malformed spec throws X::Syntax::Malformed with a `.what`
# describing the problem.

throws-like 'loop (my $i = 0; $i <= 5; $i++;) { }', X::Syntax::Malformed,
    'too many sections', what => /^'loop spec'/;
throws-like 'loop (my $i = 0; $i <= 5; $i++; $i++) { }', X::Syntax::Malformed,
    'four sections', what => /'got more'/;
throws-like 'loop () { }', X::Syntax::Malformed,
    'empty spec', what => /'semicolon'/;
throws-like 'loop (my $i = 0, $i <= 5, $i++) { }', X::Syntax::Malformed,
    'commas, one section', what => /'got 1'/;
throws-like 'loop (my $i = 0; $i <= 5, $i++) { }', X::Syntax::Malformed,
    'two sections', what => /'got 2'/;

# A well-formed C-style loop still works.
{
    my $sum = 0;
    loop (my $i = 1; $i <= 3; $i++) { $sum += $i }
    is $sum, 6, 'a valid 3-section loop runs';
}
# Nested semicolons inside brackets are not counted as section separators.
{
    my @seen;
    loop (my $i = 0; $i < 2; $i++) { @seen.push: $i }
    is @seen.elems, 2, 'nested expressions in a valid loop are unaffected';
}
# An infinite `loop { }` (no spec) is still fine.
{
    my $n = 0;
    loop { last if $n >= 2; $n++ }
    is $n, 2, 'infinite loop with no spec still works';
}
