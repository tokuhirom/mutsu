use Test;

# A chain whose middle operand is a plain variable or literal reads it in
# place instead of through a temp (MCP::Server::Tool::Web's
# `(0x41 <= $ord <= 0x5A)`); the answers must not change.

plan 10;

my $ord = 0x50;
ok (0x41 <= $ord <= 0x5A), 'in range';
$ord = 0x30;
nok (0x41 <= $ord <= 0x5A), 'below range';
$ord = 0x60;
nok (0x41 <= $ord <= 0x5A), 'above range';

my ($a, $b, $c, $d) = 1, 2, 3, 4;
ok $a < $b < $c < $d, 'four-operand chain of variables';
nok $a < $c < $b < $d, 'four-operand chain fails in the middle';
ok 1 < 2 <= 2 < 3, 'literal middles';

my @calls;
sub m($v) { @calls.push($v); $v }
ok 1 < m(2) < m(3) < 4, 'a call middle';
is @calls.join(','), '2,3', 'each call middle runs once';

my $n = 0;
nok 5 < $n < 10, 'short-circuits on the first comparison';
my $x = 3;
ok 'a' lt 'b' lt 'c' && 1 <= $x <= 3 && $x == 3, 'string and numeric chains';
