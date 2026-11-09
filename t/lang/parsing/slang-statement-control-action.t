use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's actions role may override the action of a core `statement-control`
# production (`method statement-control:sym<given>`), `callsame` into the core
# node and attach a different one (#13370, Qwiratry::Query::Slang::Topic).

plan 5;

use GivenOverrideSlang;

my @*TRACE;
my $r;
given 3 { $r = $_ * 2 }
is $r, 6, 'the rewritten given still runs its block with the topic';
is-deeply @*TRACE, ['given 3'], 'the override saw the statement';

for 1..2 { given $_ { $r += $_ } }
is $r, 9, 'a given nested in a loop body is rewritten too';
is-deeply @*TRACE, ['given 3', 'given 1', 'given 2'], 'each given went through the override';

# A postfix `given` is not the `statement-control` production.
$r = 0;
$r += $_ given 4;
is-deeply @*TRACE, ['given 3', 'given 1', 'given 2'], 'a statement modifier is left alone';
