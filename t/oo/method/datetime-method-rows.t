use Test;

plan 53;

# DateTime's methods are built-in method rows (ADR-11276 slice 3D). Both an
# inline receiver and a variable one must reach the same handler, and a
# subclass (which has no row) must still answer from the same implementation.
# Every expectation below was checked against Rakudo.
my $dt = DateTime.new(2024, 3, 5, 7, 8, 9.5, :timezone(5400));

is DateTime.new(2024, 3, 5, 7, 8, 9).hour, 7, 'inline hour';
is $dt.hour, 7, 'variable hour';
is $dt.minute, 8, 'minute';
is $dt.second, 9.5, 'second of a fractional second';
is-deeply DateTime.new(2024, 3, 5, 7, 8, 9).second, 9, 'second of a whole second is an Int';
is $dt.whole-second, 9, 'whole-second';
is $dt.hh-mm-ss, '07:08:09', 'hh-mm-ss';
is $dt.timezone, 5400, 'timezone';
is $dt.offset, 5400, 'offset';
is-deeply $dt.offset-in-hours, 1.5, 'offset-in-hours';
is-deeply $dt.offset-in-minutes, 90.0, 'offset-in-minutes is a Rat, a whole one included';
is-deeply DateTime.new(2024, 3, 5, 7, 8, 9, :timezone(7230)).offset-in-minutes, 120.5, 'offset-in-minutes of a fractional offset';
is $dt.posix, 1709617089, 'posix';

is $dt.day-of-week, 2, 'day-of-week';
is $dt.day-of-month, 5, 'day-of-month';
is $dt.day-of-year, 65, 'day-of-year';
is $dt.daycount, 60374, 'daycount is the local day count';
is $dt.days-in-month, 31, 'days-in-month';
is $dt.days-in-year, 366, 'days-in-year';
is-deeply $dt.is-leap-year, True, 'is-leap-year';
is-deeply $dt.week, (2024, 10), 'week';
is $dt.week-number, 10, 'week-number';
is $dt.week-year, 2024, 'week-year';
is $dt.weekday-of-month, 1, 'weekday-of-month';
is $dt.yyyy-mm-dd, '2024-03-05', 'yyyy-mm-dd';
is $dt.mm-dd-yyyy('/'), '03/05/2024', 'mm-dd-yyyy with a separator';
is $dt.dd-mm-yyyy, '05-03-2024', 'dd-mm-yyyy';
is $dt.mm-dd, '03-05', 'mm-dd';
is $dt.yyyy-mm('.'), '2024.03', 'yyyy-mm with a separator';

is-deeply $dt.day-fraction, <51379/172800>, 'day-fraction is of the local day';

# The Julian dates count from the instant in UTC, whatever the offset: 07:08:09.5
# at +01:30 is 05:38:09.5Z.
is-deeply $dt.modified-julian-date, <10432667779/172800>, 'modified-julian-date is of the UTC instant';
is-deeply $dt.julian-date, <425152754179/172800>, 'julian-date is of the UTC instant';
is-deeply $dt.utc.modified-julian-date, $dt.modified-julian-date, 'the same instant has the same Julian date';

is $dt.utc, DateTime.new(2024, 3, 5, 5, 38, 9.5), 'utc';
is $dt.utc.timezone, 0, 'utc has no offset';
is $dt.Date, Date.new(2024, 3, 5), 'DateTime.Date is the local date';
is $dt.DateTime, $dt, 'DateTime.DateTime is the value';
is $dt.Instant, Instant.from-posix(1709617089.5), 'Instant';
is $dt.Numeric, Instant.from-posix(1709617089.5), 'DateTime.Numeric is the Instant';
is $dt.Real, Instant.from-posix(1709617089.5), 'DateTime.Real is the Instant';
is $dt.WHICH.Str, 'DateTime|2024-03-05T07:08:09.500000+01:30', 'WHICH names the value by its ISO 8601 form';
is $dt.raku, 'DateTime.new(2024,3,5,7,8,9.5,:timezone(5400))', 'raku';
is $dt.Str, '2024-03-05T07:08:09.500000+01:30', 'Str';
is $dt.gist, '2024-03-05T07:08:09.500000+01:30', 'gist';
is-deeply $dt.formatter, Callable, 'a value without a formatter answers the Callable type object';

# Two values naming one instant at different offsets are ==, leap second included.
ok DateTime.new('2016-12-31T23:59:60Z') == DateTime.new('2017-01-01T00:59:60+01:00'), 'a leap second at another offset is ==';
ok DateTime.new(2024, 3, 5, 7, 8, 9) == DateTime.new(2024, 3, 5, 9, 8, 9, :timezone(7200)), 'one instant at two offsets is ==';

# A value with a formatter renders through it, and utc keeps it.
my $f = DateTime.new(2024, 3, 5, 7, 8, 9, :formatter({ "<{.hour}h>" }));
is $f.Str, '<7h>', 'Str runs the formatter';
is $f.utc.Str, '<7h>', 'utc keeps the formatter';

# DateTime has no Int and no weekday.
dies-ok { $dt.Int }, 'DateTime has no Int';
dies-ok { $dt.weekday }, 'DateTime has no weekday';

# A subclass has no row and still answers from the same implementation.
class MyDateTime is DateTime { }
my $m = MyDateTime.new(2024, 3, 5, 7, 8, 9);
is $m.day-of-week, 2, 'a subclass answers day-of-week';
is $m.hh-mm-ss, '07:08:09', 'a subclass answers hh-mm-ss';
