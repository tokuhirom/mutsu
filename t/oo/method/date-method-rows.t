use Test;

plan 60;

# Date's methods are built-in method rows (ADR-11276 slice 3D). Both an inline
# receiver and a variable one must reach the same handler, and a subclass (which
# has no row) must still answer from the same implementation. Every expectation
# below was checked against Rakudo.
my $d = Date.new(2024, 3, 5);

is Date.new(2024, 3, 5).day-of-week, 2, 'inline day-of-week';
is $d.day-of-week, 2, 'variable day-of-week';
is $d.day-of-month, 5, 'day-of-month';
is $d.day-of-year, 65, 'day-of-year';
is $d.daycount, 60374, 'daycount';
is $d.days-in-month, 31, 'days-in-month';
is $d.days-in-year, 366, 'days-in-year of a leap year';
is Date.new(2023, 3, 5).days-in-year, 365, 'days-in-year of another year';
is-deeply $d.is-leap-year, True, 'is-leap-year';
is-deeply Date.new(2023, 1, 1).is-leap-year, False, 'is-leap-year of another year';
is-deeply $d.week, (2024, 10), 'week is the ISO week-year and number';
is $d.week-number, 10, 'week-number';
is $d.week-year, 2024, 'week-year';
is Date.new(2021, 1, 1).week-year, 2020, 'week-year follows the ISO week, not the calendar year';
is $d.weekday-of-month, 1, 'weekday-of-month';
is Date.new(2024, 3, 31).weekday-of-month, 5, 'weekday-of-month of a fifth weekday';

is $d.yyyy-mm-dd, '2024-03-05', 'yyyy-mm-dd';
is $d.mm-dd-yyyy, '03-05-2024', 'mm-dd-yyyy';
is $d.dd-mm-yyyy, '05-03-2024', 'dd-mm-yyyy';
is $d.mm-dd, '03-05', 'mm-dd';
is $d.yyyy-mm, '2024-03', 'yyyy-mm';
is $d.yyyy-mm-dd('/'), '2024/03/05', 'yyyy-mm-dd with a separator';
is $d.mm-dd-yyyy('.'), '03.05.2024', 'mm-dd-yyyy with a separator';
is $d.dd-mm-yyyy('.'), '05.03.2024', 'dd-mm-yyyy with a separator';
is $d.mm-dd('/'), '03/05', 'mm-dd with a separator';
is $d.yyyy-mm('/'), '2024/03', 'yyyy-mm with a separator';

is $d.succ, Date.new(2024, 3, 6), 'succ';
is $d.pred, Date.new(2024, 3, 4), 'pred';
is Date.new(2024, 12, 31).succ, Date.new(2025, 1, 1), 'succ carries into the next year';
is $d.first-date-in-month, Date.new(2024, 3, 1), 'first-date-in-month';
is $d.last-date-in-month, Date.new(2024, 3, 31), 'last-date-in-month';
is Date.new(2024, 2, 10).last-date-in-month, Date.new(2024, 2, 29), 'last-date-in-month of February in a leap year';

is $d.Date, $d, 'Date.Date is the date';
is $d.DateTime, DateTime.new(2024, 3, 5, 0, 0, 0), 'Date.DateTime is midnight UTC';
is $d.Int, 60374, 'Date.Int is the day count';
is $d.Numeric, 60374, 'Date.Numeric is the day count';
is $d.Real, 60374, 'Date.Real is the day count';
is $d.WHICH.Str, 'Date|60374', 'WHICH names the date by its day count';
is $d.raku, 'Date.new(2024,3,5)', 'raku';
is $d.Str, '2024-03-05', 'Str';
is $d.gist, '2024-03-05', 'gist';
is-deeply $d.formatter, Callable, 'a date without a formatter answers the Callable type object';

# A date with a formatter renders through it, and the derived dates keep it.
my $f = Date.new(2024, 3, 5, :formatter({ "<{.year}/{.month}>" }));
is $f.Str, '<2024/3>', 'Str runs the formatter';
is $f.gist, '<2024/3>', 'gist runs the formatter';
is $f.succ.Str, '<2024/3>', 'succ keeps the formatter';
is $f.pred.Str, '<2024/3>', 'pred keeps the formatter';
is $f.first-date-in-month.Str, '<2024/3>', 'first-date-in-month keeps the formatter';
is $f.Date.Str, '<2024/3>', 'Date.Date keeps the formatter';
is $f.yyyy-mm-dd, '2024-03-05', 'the formatter does not change yyyy-mm-dd';
ok $f.formatter.defined, 'formatter answers the stored Callable';

# Date.Int and friends are gone from the posix timestamp: the day count is
# what Rakudo answers, and what a Date compares by.
ok Date.new(2024, 3, 5) == Date.new(2024, 3, 5), 'two equal dates are ==';
nok Date.new(2024, 3, 5) == Date.new(2024, 3, 6), 'two different dates are not ==';
is Date.new(2024, 3, 6) - Date.new(2024, 3, 5), 1, 'the difference of two dates is a day count';

# Methods Rakudo does not declare on Date are not answered.
dies-ok { $d.weekday }, 'Date has no weekday';
dies-ok { $d.Instant }, 'Date has no Instant';

# A subclass has no row and still answers from the same implementation.
class MyDate is Date { }
my $m = MyDate.new(2024, 3, 5);
is $m.day-of-week, 2, 'a subclass answers day-of-week';
is $m.week-number, 10, 'a subclass answers week-number';
is $m.yyyy-mm-dd, '2024-03-05', 'a subclass answers yyyy-mm-dd';
is $m.Int, 60374, 'a subclass answers Int';

# A user method still wins over the row.
class Overridden is Date { method day-of-week { 99 } }
is Overridden.new(2024, 3, 5).day-of-week, 99, 'a user method wins';
