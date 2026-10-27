use Test;

plan 3;

# A statement-form FIRST/NEXT/LAST (`NEXT STATEMENT;`) shares the enclosing
# block's lexical scope: a variable it declares is visible to the other
# phasers written in the same block.
my @log;
for 1..3 {
    NEXT (state $seen) += $_;
    LAST @log.push($seen);
    FIRST @log.push('first');
}
is-deeply @log, ['first', 6], 'bare FIRST/NEXT/LAST run, NEXT declares a state shared with LAST';

my $n = 0;
for 1..2 { NEXT $n++; }
is $n, 2, 'a bare NEXT runs once per iteration';

# The block form keeps its own scope.
my @blocks;
for 1..2 { NEXT { @blocks.push($_) } }
is-deeply @blocks, [1, 2], 'the block form runs too';
