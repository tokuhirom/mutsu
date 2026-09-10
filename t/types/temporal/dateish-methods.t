use Test;

plan 11;

# Dateish methods that were missing on DateTime (Date already had the ordered
# formatters) or on both types (days-in-year, formatter).

# Ordered date formatters on DateTime, with the default and a custom separator.
my $dt = DateTime.new(1470853583);
is $dt.yyyy-mm-dd,      '2016-08-10', 'DateTime.yyyy-mm-dd';
is $dt.mm-dd-yyyy,      '08-10-2016', 'DateTime.mm-dd-yyyy';
is $dt.dd-mm-yyyy,      '10-08-2016', 'DateTime.dd-mm-yyyy';
is $dt.yyyy-mm-dd('/'), '2016/08/10', 'DateTime.yyyy-mm-dd with a separator';

# Date still works.
is Date.new('2015-11-15').yyyy-mm-dd,      '2015-11-15', 'Date.yyyy-mm-dd';
is Date.new('2015-11-15').dd-mm-yyyy('/'), '15/11/2015', 'Date.dd-mm-yyyy with a separator';

# days-in-year on both types.
is Date.new('2016-01-02').days-in-year, 366, 'Date.days-in-year (leap)';
is DateTime.new(:year<2100>, :month<2>).days-in-year, 365, 'DateTime.days-in-year (non-leap)';

# .formatter returns the Callable default or the supplied formatter.
is Date.new('2015-12-31').formatter.^name, 'Callable', 'default formatter is Callable';
my $us = sub ($self) { sprintf "%02d/%02d/%04d", .month, .day, .year given $self };
my $d = Date.new('2015-12-31', formatter => $us);
is $d.formatter.^name, 'Sub', 'a supplied formatter keeps its type';
is $d.Str, '12/31/2015', 'the formatter renders the date';
