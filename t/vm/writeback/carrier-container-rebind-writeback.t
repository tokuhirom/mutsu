use Test;

plan 6;

# A container REBIND inside a test-function block (`lives-ok { $parsed = ... }`)
# must reach the caller's slot even when the slot already holds a container:
# the second Hash assignment was silently dropped (the carrier writeback
# protected container slots wholesale), so JSON::Marshal's
# t/030-trait.t / t/080-type-constraints.t re-parses kept the FIRST result.
my $key = "one";
my %h = one => { x => 1 }, two => { x => 2 };
my $parsed;
lives-ok { $parsed = %h{$key} }, 'first assignment lives';
is $parsed<x>, 1, 'first hash value arrived';
$key = "two";
lives-ok { $parsed = %h{$key} }, 'second assignment lives';
is $parsed<x>, 2, 'the REBOUND hash value arrived (not the stale first)';

# Arrays too.
my $sel = "a";
my %arrs = a => [1, 2], b => [3, 4];
my $got;
lives-ok { $got = %arrs{$sel} }, 'array pick lives';
$sel = "b";
lives-ok { $got = %arrs{$sel} }, 'second array pick lives';

done-testing;
