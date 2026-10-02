use Test;

# From Date::Calendar::Strftime: `%V` is only filled in when
# `$date.can('week-number')`, which used to be False for Date/DateTime.
plan 3;

my $d  = Date.new('2001-02-01');
my $dt = DateTime.new('2001-02-01T10:20:30Z');
my @dateish = <week week-number week-year weekday-of-month days-in-month
               is-leap-year day-of-year day-of-month earlier later truncated-to>;

ok so(@dateish.all.&{ $d.can($_) }), 'Date.can answers the Dateish methods';
ok so((|@dateish, |<posix utc local whole-second in-timezone julian-date
                    modified-julian-date>).all.&{ $dt.can($_) }),
    'DateTime.can answers the Dateish and DateTime methods';
nok $d.can('julian-date'), 'Date has no julian-date';
