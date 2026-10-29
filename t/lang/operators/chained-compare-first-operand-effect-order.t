use Test;

plan 6;

my @a = 5, 6;
ok (@a[0] = 0) == @a[0] == 0, 'an element assignment as the first operand runs before the middle one is read';

my $x = 5;
ok ($x = 0) == $x == 0, 'a scalar assignment as the first operand runs before the middle one is read';

my @log;
sub op($tag, $v) { @log.push($tag); $v }
ok op('a', 1) < op('b', 2) < op('c', 3), 'a chain of calls holds';
is @log.join(','), 'a,b,c', 'operands evaluate left to right';

my $y = 1;
ok !(($y = 3) < $y < 9), 'the first assignment is visible to the middle operand';
is $y, 3, 'and the assignment happened once';
