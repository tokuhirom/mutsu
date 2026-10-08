use Test;

plan 36;

# Date's and DateTime's `later`, `earlier`, `truncated-to`, `in-timezone` and
# `local` are built-in method rows (ADR-11276 §9.34). A plain receiver reaches
# the row; a subclass (no shape) and the spelling a row does not bind (the
# units as a positional list of pairs) reach the same implementation through
# the cascade. Every expectation below was checked against Rakudo.
my $d  = Date.new(2024, 1, 31);
my $dt = DateTime.new(2024, 3, 5, 7, 8, 9.5, :timezone(3600));
class MyDate is Date { }
class MyDT is DateTime { }
my $mdt = MyDT.new(2024, 3, 5, 7, 8, 9, :timezone(3600));

# later / earlier
is $d.later(:1month), '2024-02-29', 'Date.later clips the day to the month';
is Date.new(2024, 1, 31).later(:1month), '2024-02-29', 'inline receiver';
is $d.earlier(:1year), '2023-01-31', 'Date.earlier';
is $d.later((:2days, :1week)), '2024-02-09', 'units as a list of pairs, applied in order';
is $dt.later(:90minutes), '2024-03-05T08:38:09.500000+01:00', 'DateTime.later';
is $dt.earlier((:2hours, :30seconds)), '2024-03-05T05:07:39.500000+01:00', 'DateTime.earlier';
is $dt.later((:1day, :1month)), '2024-04-06T07:08:09.500000+01:00', 'DateTime.later, two units';
is MyDate.new(2024, 1, 31).later(:1month).^name, 'MyDate', 'a Date subclass keeps its class';
is $mdt.later(:1hour).^name, 'MyDT', 'a DateTime subclass keeps its class';
throws-like { $d.later(:3fortnights) }, Exception, 'an unknown unit fails';

# truncated-to
is $d.truncated-to('month'), '2024-01-01', 'Date.truncated-to month';
is $d.truncated-to('week'), '2024-01-29', 'Date.truncated-to week is the Monday';
is $dt.truncated-to('hour'), '2024-03-05T07:00:00+01:00', 'DateTime.truncated-to hour';
is $dt.truncated-to('day'), '2024-03-05T00:00:00+01:00', 'DateTime.truncated-to day keeps the offset';
is $mdt.truncated-to('day'), '2024-03-05T00:00:00+01:00', 'subclass truncated-to';
is $mdt.truncated-to('day').^name, 'MyDT', 'subclass keeps its class';
throws-like { $d.truncated-to('decade') }, Exception, 'an unknown truncation unit fails';

# in-timezone
is $dt.in-timezone(0), '2024-03-05T06:08:09.500000Z', 'in-timezone to UTC';
is $dt.in-timezone(-18000), '2024-03-05T01:08:09.500000-05:00', 'in-timezone to a negative offset';
is $dt.utc, '2024-03-05T06:08:09.500000Z', 'utc';
is $mdt.in-timezone(0).^name, 'MyDT', 'subclass in-timezone keeps its class';
is $dt.in-timezone(3600).timezone, 3600, 'the offset is the argument';

# local reads $*TZ
{
    my $*TZ = 7200;
    is $dt.local, '2024-03-05T08:08:09.500000+02:00', 'local';
    is $mdt.local, '2024-03-05T08:08:09+02:00', 'subclass local';
    is $mdt.local.^name, 'MyDT', 'subclass local keeps its class';
    is $dt.local.timezone, 7200, 'local takes the $*TZ offset';
}
{
    my $*TZ = 0;
    is $dt.local, '2024-03-05T06:08:09.500000Z', 'local with a zero $*TZ';
}

# the results are fresh values: the receiver is unchanged
is $dt, '2024-03-05T07:08:09.500000+01:00', 'the receiver DateTime is unchanged';
is $d, '2024-01-31', 'the receiver Date is unchanged';
is $d.later(:1day).earlier(:1day), $d, 'later then earlier round-trips';

# several named units have no order of application: Rakudo refuses them,
# a positional list of pairs fixes the order and stays legal
class MyD is Date {}
throws-like { $d.later(:1month, :2days) }, Exception,
    message => /'More than one time unit supplied'/, 'Date.later with two named units';
throws-like { $d.earlier(:1week, :1day) }, Exception,
    message => /'More than one time unit supplied'/, 'Date.earlier with two named units';
throws-like { $dt.later(:1hour, :30minutes) }, Exception,
    message => /'More than one time unit supplied'/, 'DateTime.later with two named units';
throws-like { MyD.new(2024, 1, 31).later(:1month, :2days) }, Exception,
    message => /'More than one time unit supplied'/, 'subclass later with two named units';
is $d.later((:1month, :2days)), '2024-03-02', 'a list of pairs keeps working';
is $d.later(:1month), '2024-02-29', 'a single unit keeps working';
