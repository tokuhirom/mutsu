use lib 't/lib';
use Test;

# A method of a class nested in a `unit class` keeps its writes to the unit's
# file-scope `my` variables across calls (#16843).
plan 8;

use UnitClassNestedLexical;

my $o = make;
is $o.count ~ $o.count ~ $o.count, '123', 'a scalar counter advances across calls';
is $o.log('a') ~ $o.log('b'), '12', '.push on an array keeps growing';
is $o.seen('x') ~ $o.seen('x') ~ $o.seen('y'), '121', '++%h{$k} accumulates per key';
is $o.set-seen('z'), 10, 'an element store is visible within the call';
is $o.by ~ $o.by ~ $o.by, '123', 'a private method counts across calls';
is state-report, '3 2 3', 'the unit class\'s own routine sees the nested class\'s writes';
is make.count, 4, 'a fresh instance continues the same counter';
is $o.seen('x'), 3, 'and the same hash';
