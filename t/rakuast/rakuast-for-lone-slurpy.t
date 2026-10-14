use v6;
use experimental :rakuast;
use Test;

# A `for` pointy block whose only parameter is a slurpy keeps zero positional
# slots after the round trip: `-> *%h` takes no positional, so it dies.

plan 4;

my $ran = 0;
try EVAL(Q{for 1, 2 -> *%h { $ran++ }}.AST);
is $ran, 0, '`-> *%h` never runs the body';
ok $!.defined, '... and the loop dies';

my @seen;
EVAL(Q{for 1, 2 -> *@a { @seen.push(@a.elems) }}.AST);
is-deeply @seen, [1, 1], '`-> *@a` receives one item per iteration';

my @each;
EVAL(Q{for 1, 2 -> $x { @each.push($x) }}.AST);
is-deeply @each, [1, 2], 'a single scalar parameter still iterates';
