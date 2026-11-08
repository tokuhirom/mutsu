use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang grammar role's `method statementlist` wraps the core rule (ADR-16888):
# the wrapper runs for every statement list that starts after the `use`, not
# for the list holding the `use` itself, and delegating to the core rule does
# not re-enter the wrapper.

plan 6;

use StatementListWrapperSlang;

is lists-seen(), 0, 'the list holding the use is not wrapped';

if True { my $a = 1 }
is lists-seen(), 1, 'a block body is a wrapped list';

if True { if True { my $b = 2 } }
is lists-seen(), 3, 'nested blocks are wrapped once each';

sub f { 42 }
is f(), 42, 'a sub body still answers its value';
is lists-seen(), 4, 'the sub body is wrapped';

my $seen = lists-seen();
for 1..3 { my $x = $_ }
is lists-seen() - $seen, 3, "the loop body is wrapped, and its note runs on each of three iterations";
