use Test;

# A Date/DateTime `:formatter` runs against the value being stringified: values
# derived by a conversion keep the formatter and render their own fields, and
# every string context (prefix ~, interpolation, infix ~, .Str/.gist) agrees.
# GH #9890 (DateTime::Format t/030-rfc2822.rakutest test 2).

plan 36;

class HourFmt does Callable {
    method CALL-ME($dt) {
        sprintf '%02d:%02d %+05d', $dt.hour, $dt.minute, $dt.offset div 36
    }
}

my $fmt = HourFmt.new;
my $dt = DateTime.new(:year(1582), :month(10), :day(4), :hour(13), :minute(2),
    :second(3.654321), :timezone(-28800), :formatter($fmt));

is ~$dt, '13:02 -0800', 'object formatter renders the original';
is ~$dt.utc, '21:02 +0000', '~ on .utc renders the converted value';
is $dt.utc.Str, '21:02 +0000', '.utc.Str';
is $dt.utc.gist, '21:02 +0000', '.utc.gist';
is "{$dt.utc}", '21:02 +0000', 'interpolated .utc';
is $dt.utc ~ '!', '21:02 +0000!', 'infix ~ on .utc';
ok $dt.utc.formatter === $fmt, '.utc keeps the same formatter object';
is ~$dt.in-timezone(3600), '22:02 +0100', '.in-timezone';
is ~$dt.later(:1hour), '14:02 -0800', '.later';
is ~$dt.earlier(:1hour), '12:02 -0800', '.earlier';
is ~$dt.truncated-to('day'), '00:00 -0800', '.truncated-to';
is ~$dt.clone(:hour(3)), '03:02 -0800', '.clone(:hour) re-renders';
is ~$dt.DateTime, '13:02 -0800', '.DateTime is the invocant';
is ~($dt.clone(:second(3)) + Duration.new(60)), '1582-10-04T13:03:03-08:00',
    'DateTime + Duration does not carry the formatter';
is ~$dt.Date, '1582-10-04', '.Date does not carry the formatter';

{
    my $day = { sprintf '%02d.%02d.%04d', .day, .month, .year };
    my $d = Date.new(2020, 1, 2, :formatter($day));
    is ~$d, '02.01.2020', 'Date formatter';
    is ~$d.succ, '03.01.2020', 'Date.succ';
    is ~$d.pred, '01.01.2020', 'Date.pred';
    is ~($d + 3), '05.01.2020', 'Date + Int re-renders';
    is ~($d - 1), '01.01.2020', 'Date - Int re-renders';
    is ~$d.later(:1month), '02.02.2020', 'Date.later';
    is ~$d.earlier(:1day), '01.01.2020', 'Date.earlier';
    is ~$d.clone(:day(5)), '05.01.2020', 'Date.clone(:day) re-renders';
    is ~$d.truncated-to('month'), '01.01.2020', 'Date.truncated-to';
    is ~$d.Date, '02.01.2020', 'Date.Date is the invocant';
    is ~$d.last-date-in-month, '31.01.2020', 'Date.last-date-in-month';
    is "$d", '02.01.2020', 'interpolated Date';
    is ~Date.new($d), '02.01.2020', 'Date.new(Date) inherits the formatter';
    my $dt-day = DateTime.new(:2021year, :7day, :formatter({ 'day ' ~ .day }));
    is ~Date.new($dt-day), 'day 7', 'Date.new(DateTime) inherits the formatter';
    is ~Date.new($d, :formatter({ 'own' })), 'own', 'an explicit formatter wins';
}

{
    my $counter = 0;
    my $d = DateTime.new(:2020year, :formatter({ $counter++; ~.year }));
    is ~$d, '2020', 'first stringification';
    is ~$d, '2020', 'second stringification';
    is $counter, 2, 'the formatter runs on every stringification';
}

{
    class MyDT is DateTime { }
    my $m = MyDT.new(:2020year, :5hour, :formatter($fmt));
    isa-ok $m.utc, MyDT, '.utc keeps a DateTime subclass';
    is ~$m.utc, '05:00 +0000', 'subclass .utc keeps the formatter';
    is ~$m.in-timezone(3600), '06:00 +0100', 'subclass .in-timezone keeps the formatter';
}
