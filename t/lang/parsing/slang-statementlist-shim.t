use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang token that calls the core `<statementlist>` rule gets a
# RakuAST::StatementList for the statements it matched (ADR-16888).

plan 3;

use StatementListShimSlang;

my @log;
group { @log.push: 1; @log.push: 2 }
is @log.join(','), '1,2', 'the statements of the list run';

my $v = 0;
group { $v = 10; $v += 5 }
is $v, 15, 'several statements, in order';

group { group { @log.push: 3 } }
is @log.join(','), '1,2,3', 'a list can hold another group';
