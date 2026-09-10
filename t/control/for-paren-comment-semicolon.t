use v6;
use Test;

# The obsolete C-style `for (init; test; incr)` detector scans the paren group
# for a top-level `;` — a semicolon inside a `#` line comment is prose, not a
# separator (Text::CSV's t/80_diag.t annotates its fragment-spec list with
# comments like "cell has no ;").

plan 2;

my @got;
for (1,   # has no ;
     2,   # another; note
     3) { @got.push($_) }
is-deeply @got, [1, 2, 3], 'comment semicolons inside for (...) are ignored';

dies-ok { EVAL q[for (my $i = 0; $i < 3; $i++) { }] },
    'a real C-style for is still rejected';
