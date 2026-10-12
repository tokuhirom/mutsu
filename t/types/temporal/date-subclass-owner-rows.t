use Test;

# ADR-11276 §9.87: an instance of a user subclass of `Date`/`DateTime` has no
# dispatch shape; its argless methods are answered by the rows of `Date` /
# `DateTime` through their owner, the instance as invocant.

plan 67;

class MD is Date {}
class MDT is DateTime {}
my $d = MD.new(2024, 2, 29);
my $dt = MDT.new(2024, 2, 29, 13, 45, 30.5, :timezone(3600));

is $d.year.raku, '2024', 'MD.year';
is $d.month.raku, '2', 'MD.month';
is $d.day.raku, '29', 'MD.day';
is $d."day-of-month"().raku, '29', 'MD.day-of-month';
is $d."day-of-week"().raku, '4', 'MD.day-of-week';
is $d."day-of-year"().raku, '60', 'MD.day-of-year';
is $d."is-leap-year"().raku, 'Bool::True', 'MD.is-leap-year';
is $d."days-in-month"().raku, '29', 'MD.days-in-month';
is $d."days-in-year"().raku, '366', 'MD.days-in-year';
is $d.formatter.raku, 'Callable', 'MD.formatter';
is $d.daycount.raku, '60369', 'MD.daycount';
is $d.Str.raku, '"2024-02-29"', 'MD.Str';
is $d.gist.raku, '"2024-02-29"', 'MD.gist';
is $d.Date.raku, 'Date.new(2024,2,29)', 'MD.Date';
is $d."yyyy-mm-dd"().raku, '"2024-02-29"', 'MD.yyyy-mm-dd';
is $d."mm-dd-yyyy"().raku, '"02-29-2024"', 'MD.mm-dd-yyyy';
is $d."dd-mm-yyyy"().raku, '"29-02-2024"', 'MD.dd-mm-yyyy';
is $d."mm-dd"().raku, '"02-29"', 'MD.mm-dd';
is $d."yyyy-mm"().raku, '"2024-02"', 'MD.yyyy-mm';
is $d."week-year"().raku, '2024', 'MD.week-year';
is $d."week-number"().raku, '9', 'MD.week-number';
is $d.week.raku, '(2024, 9)', 'MD.week';
is $d."weekday-of-month"().raku, '5', 'MD.weekday-of-month';
is $d.Int.raku, '60369', 'MD.Int';
is $d.Numeric.raku, '60369', 'MD.Numeric';
is $d.Real.raku, '60369', 'MD.Real';
is $d.DateTime.raku, 'DateTime.new(2024,2,29,0,0,0)', 'MD.DateTime';
is $dt.year.raku, '2024', 'MDT.year';
is $dt.month.raku, '2', 'MDT.month';
is $dt.day.raku, '29', 'MDT.day';
is $dt.hour.raku, '13', 'MDT.hour';
is $dt.minute.raku, '45', 'MDT.minute';
is $dt."day-of-month"().raku, '29', 'MDT.day-of-month';
is $dt.second.raku, '30.5', 'MDT.second';
is $dt.timezone.raku, '3600', 'MDT.timezone';
is $dt.offset.raku, '3600', 'MDT.offset';
is $dt."offset-in-hours"().raku, '1.0', 'MDT.offset-in-hours';
is $dt."offset-in-minutes"().raku, '60.0', 'MDT.offset-in-minutes';
is $dt."day-of-week"().raku, '4', 'MDT.day-of-week';
is $dt."day-of-year"().raku, '60', 'MDT.day-of-year';
is $dt."is-leap-year"().raku, 'Bool::True', 'MDT.is-leap-year';
is $dt."days-in-month"().raku, '29', 'MDT.days-in-month';
is $dt."days-in-year"().raku, '366', 'MDT.days-in-year';
is $dt.formatter.raku, 'Callable', 'MDT.formatter';
is $dt."yyyy-mm-dd"().raku, '"2024-02-29"', 'MDT.yyyy-mm-dd';
is $dt."mm-dd-yyyy"().raku, '"02-29-2024"', 'MDT.mm-dd-yyyy';
is $dt."dd-mm-yyyy"().raku, '"29-02-2024"', 'MDT.dd-mm-yyyy';
is $dt."mm-dd"().raku, '"02-29"', 'MDT.mm-dd';
is $dt."yyyy-mm"().raku, '"2024-02"', 'MDT.yyyy-mm';
is $dt.daycount.raku, '60369', 'MDT.daycount';
is $dt."whole-second"().raku, '30', 'MDT.whole-second';
is $dt."hh-mm-ss"().raku, '"13:45:30"', 'MDT.hh-mm-ss';
is $dt.Str.raku, '"2024-02-29T13:45:30.500000+01:00"', 'MDT.Str';
is $dt.gist.raku, '"2024-02-29T13:45:30.500000+01:00"', 'MDT.gist';
is $dt.Date.raku, 'Date.new(2024,2,29)', 'MDT.Date';
is $dt.posix.raku, '1709210730', 'MDT.posix';
is $dt."week-year"().raku, '2024', 'MDT.week-year';
is $dt."week-number"().raku, '9', 'MDT.week-number';
is $dt.week.raku, '(2024, 9)', 'MDT.week';
is $dt."weekday-of-month"().raku, '5', 'MDT.weekday-of-month';
is $dt."julian-date"().raku, '<425151941461/172800>', 'MDT.julian-date';
is $dt."modified-julian-date"().raku, '<10431855061/172800>', 'MDT.modified-julian-date';
is $dt."day-fraction"().raku, '<99061/172800>', 'MDT.day-fraction';
is $dt.Instant.raku, 'Instant.from-posix(1709210730.5)', 'MDT.Instant';
is $dt.Numeric.raku, 'Instant.from-posix(1709210730.5)', 'MDT.Numeric';
is $dt.Real.raku, 'Instant.from-posix(1709210730.5)', 'MDT.Real';
is $dt.DateTime.raku, 'DateTime.new(2024,2,29,13,45,30.5,:timezone(3600))', 'MDT.DateTime';
