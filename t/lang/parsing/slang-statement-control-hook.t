use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's grammar role may add a new `statement-control:sym<...>` production
# (ADR-15762, #13370): the slang's token and action run where the parser starts
# a statement, and the RakuAST node the action attaches stands for it.

plan 7;

use StatementControlSlang;

my @log;
twice { @log.push('a') }
is @log.join(','), 'a,a', 'the block of a slang statement runs through the attached call';

# A block with its own statements and a closure over the surrounding scope.
my $n = 0;
twice { $n += 10; $n++ }
is $n, 22, 'the block closes over the surrounding lexical scope';

# The slang statement inside a block of another one.
my $count = 0;
twice { twice { $count++ } }
is $count, 4, 'a slang statement nests in the block of another';

# The keyword is a whole word: other identifiers stay ordinary.
sub twiceish($x) { $x * 3 }
is twiceish(2), 6, 'an identifier that starts with the keyword is not the statement';

# Ordinary statements around it are unaffected.
my $seen = '';
for 1..2 { $seen ~= $_ }
twice { $seen ~= 'x' }
is $seen, '12xx', 'statements before and after parse as usual';

# A slang statement in a routine body.
sub run-it { my $c = 0; twice { $c++ }; $c }
is run-it(), 2, 'a slang statement works in a routine body';

# Slang state is lexical to the compilation unit: an EVAL string is its own
# unit and parses in the stock grammar.
throws-like 'twice { 1 }', Exception, 'an EVAL string does not see the slang';
