use Test;

plan 8;

# `EXPR when COND` is a statement modifier, like `EXPR if COND`.
my $vowels = 'aaaooouuu';
is ('all vowels' when $vowels ~~ /^ <[aeiou]>+ $/), 'all vowels', 'when modifier on a match';

# A Match on the right of a smartmatch is an already-decided result, so it passes through
# like a Bool. That is what lets the above work with no `given` to set the topic.
my $m = ('abc' ~~ /b/);
ok (42 ~~ $m), 'smartmatch against a successful Match is true';
ok (Any ~~ $m), 'smartmatch against a Match ignores the topic';
nok ('abc' ~~ ('xyz' ~~ /b/)), 'smartmatch against a failed match is false';

# `when` chains with a following `given`, which topicalizes first.
my $castle = 'phantom';
is ('Boo!' when /phantom/ given $castle), 'Boo!', 'when ... given ... chains';
# A false statement-modifier `when` is NOT a `when` clause -- Rakudo lowers it
# to a plain conditional, so it yields `Empty` rather than the clause's falsy
# smartmatch value (ADR-0052).
is-deeply ('Boo!' when /ghost/ given $castle), Empty,
    'a non-matching when modifier yields Empty';

# The topic set by an enclosing `given` block is visible to a `when` modifier.
my $found;
given 'camelia' {
    $found = 'bug' when /camelia/;
}
is $found, 'bug', 'when modifier sees the topic of an enclosing given';

# A real `when COND { ... }` statement is still a statement, not a modifier.
my $branch;
given 3 {
    when 3 { $branch = 'three' }
    default { $branch = 'other' }
}
is $branch, 'three', 'when block statement still parses as a block';
