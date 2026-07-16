use v6;
use Test;

plan 6;

# DateTime.offset-in-minutes (roast/integration/advent2010-day16.t)
my $p = DateTime.new('1963-11-23T17:15:00+0130');
is $p.offset-in-minutes, 90, 'positive offset-in-minutes';
is $p.offset-in-hours, 1.5, 'positive offset-in-hours';
is $p.gist, '1963-11-23T17:15:00+01:30', 'positive offset gist';

my $n = DateTime.new('1963-11-23T17:15:00-0145');
is $n.offset-in-minutes, -105, 'negative offset-in-minutes';
is $n.gist, '1963-11-23T17:15:00-01:45', 'negative offset gist';

# UTC (zero offset)
is DateTime.new(:year(1963)).offset-in-minutes, 0, 'utc offset-in-minutes is 0';
