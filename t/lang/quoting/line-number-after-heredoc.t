use Test;

# `$?LINE` is resolved by counting the newlines that precede the parser's
# current position. A heredoc body is parsed out of a *leaked* buffer whose
# line numbering jumps at the terminator, so the count has to be taken against
# that region's own line-jump bookkeeping rather than the file's. Both sides
# are indexed now (one pass per buffer, a binary search per lookup, instead of
# rescanning the prefix on every one of the hundreds of thousands of lookups a
# parse makes); this pins that the answers did not move.
# Verified against rakudo 2026.07.

plan 4;

my $first = q:to/END/;
alpha
beta
gamma
END
is $?LINE, 19, '$?LINE after a three-line heredoc counts the body lines';

my $second = q:to/END2/;
one
END2
is $?LINE, 24, '$?LINE after a second heredoc stays in step';

is $first.lines.elems, 3, 'the first heredoc body survived intact';
is $second.chomp, 'one', 'the second heredoc body survived intact';
