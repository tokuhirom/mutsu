use Test;
plan 14;

# Date subclass .today
{
    class Dated is Date {}
    my $d = Dated.today;
    isa-ok $d, Dated, 'Dated.today returns a Dated instance';
    isa-ok $d, Date, 'Dated.today is also a Date';
    ok $d.year > 2000, 'Dated.today has a valid year';
}

# Date subclass .new
{
    class FooDate is Date { has $.foo }
    my $fd = FooDate.new('2020-01-15', foo => 42);
    is $fd.year, 2020, 'FooDate.new has correct year';
    is $fd.month, 1, 'FooDate.new has correct month';
    is $fd.day, 15, 'FooDate.new has correct day';
    is $fd.foo, 42, 'FooDate.new has custom attribute';
}

# Date methods: yyyy-mm-dd, first-date-in-month, last-date-in-month
{
    my $d = Date.new(2020, 3, 15);
    is $d.yyyy-mm-dd, '2020-03-15', 'yyyy-mm-dd method';
    is $d.first-date-in-month, '2020-03-01', 'first-date-in-month';
    is $d.last-date-in-month, '2020-03-31', 'last-date-in-month';
}

# Date type object methods
{
    is-deeply Date.Date, Date, 'Date:U.Date returns Date';
    is-deeply Date.DateTime, DateTime, 'Date:U.DateTime returns DateTime';
}

# Date.new from Instant
{
    my $i = Instant.from-posix(1234567890);
    is Date.new($i).Str, '2009-02-13', 'Date.new(Instant) works';
}

# Date range iteration
{
    my $a = Date.new('2020-01-01');
    my $b = Date.new('2020-01-03');
    is ($a .. $b).join(' '), '2020-01-01 2020-01-02 2020-01-03', 'Date range iterates';
}
