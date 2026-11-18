use Test;

# `for ( LIST ) { ... }` is C-style only when the parenthesized group itself
# holds a top-level `;`. A `#` inside a quoting construct the C-style probe
# does not know (`q<...]#>`) is not a comment, so it must not hide the
# group's closing paren and make the loop body's `;` look like a separator
# (CSS::Specification::Compiler t/compile.t).

plan 3;

my @seen;
for (
    'a' => q<[<x> | <y> ]#>,
    'b' => q<plain>,
) {
    my $k = .key;
    @seen.push: $k;
}
is-deeply @seen, ['a', 'b'], 'the list loop runs';

throws-like 'for (my $i = 0; $i < 3; $i++) { }', X::Obsolete,
    'a real C-style for is still rejected';

my @t;
for ('x; y', 'z') { @t.push: $_ }
is-deeply @t, ['x; y', 'z'], 'a ; inside a string is not a separator';
