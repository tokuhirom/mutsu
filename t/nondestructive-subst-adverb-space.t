use v6;
use Test;

# The non-destructive substitution `S///` accepts whitespace between its adverbs
# and the opening delimiter (`S:g /pattern/replacement/`), just like the
# in-place `s///`. mutsu used to reject `S:g /.../` (with a space) as an
# "Undeclared routine: S:g".

plan 6;

# 1. S:g with a space before the delimiter.
{
    my $s = "a1b2c3";
    is (S:g /\d// given $s), 'abc', 'S:g /.../ (space) removes all digits, non-destructive';
    is $s, 'a1b2c3', 'original is untouched by S:g';
}

# 2. S:g with no space still works (regression guard).
{
    my $s = "x9y8";
    is (S:g/\d// given $s), 'xy', 'S:g/.../ (no space) still works';
}

# 3. S with a single adverb and a replacement string.
{
    my $s = "foo bar foo";
    is (S:g /foo/baz/ given $s), 'baz bar baz', 'S:g /.../.../  with replacement';
}

# 4. A different adverb (`:i`) with a space before the delimiter.
{
    my $s = "Foo FOO foo";
    is (S:i:g /foo/x/ given $s), 'x x x', 'S:i:g /.../.../  case-insensitive with space';
}

# 5. The topic form used by the real dist (inside a for/given block).
{
    my @out = gather for <a1 b2 c3> {
        take S:g /<:digit>// ;
    }
    is @out.join(','), 'a,b,c', 'S:g on the topic inside a loop';
}
