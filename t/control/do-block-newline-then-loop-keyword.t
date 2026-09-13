use v6;
use Test;

# `do BLOCK while ...` is the Perl 5 idiom Raku rejects (X::Obsolete, "please use
# repeat...while"), and the same rejection covers `until`, `for` and `given` on
# the SAME line. It does not reach the next line: a `}` that ends its line ends
# the statement, so
#
#     my $x = do { 1 }
#     for @list { ... }
#
# is two statements. mutsu skipped all whitespace before looking for the keyword,
# so it rejected that too — which is how `MetamodelX::Red::Model` (Red),
# `PDF::Font::Loader::FontObj::CID` and `App::MoarVM::HeapAnalyzer::Model` all
# failed to load, each on a `do { ... }` whose block is followed by an ordinary
# loop statement.

plan 12;

# The next line opens a new statement, and the `do` block still yields its value.
{
    my @seen;
    my $x = do { 1 }
    for 1, 2 { @seen.push: $_ }
    is $x, 1, 'do block value survives a following `for` statement';
    is @seen.join(','), '1,2', 'and the `for` ran as its own statement';
}

{
    my $i = 0;
    my $x = do { 7 }
    while $i < 2 { $i++ }
    is $x, 7, 'do block value survives a following `while` statement';
    is $i, 2, 'and the `while` ran as its own statement';
}

{
    my $i = 0;
    my $x = do { 8 }
    until $i >= 3 { $i++ }
    is $x, 8, 'do block value survives a following `until` statement';
    is $i, 3, 'and the `until` ran as its own statement';
}

{
    my $seen = '';
    my $x = do { 9 }
    given 'topic' { $seen = $_ }
    is $x, 9, 'do block value survives a following `given` statement';
    is $seen, 'topic', 'and the `given` ran as its own statement';
}

# A comment between the `}` and the newline does not change that.
{
    my @seen;
    my $x = do { 4 }   # trailing comment
    for 1, 2 { @seen.push: $_ }
    is $x, 4, 'a trailing comment still leaves two statements';
    is @seen.elems, 2, 'and the loop still ran';
}

# The same-line spellings are still the obsolete-syntax error.
throws-like 'my $i = 0; do { $i++ } while $i < 3', X::Obsolete,
    'do BLOCK while on one line is still rejected';
throws-like 'do { say $_ } for 1, 2', X::Obsolete,
    'do BLOCK for on one line is still rejected';
