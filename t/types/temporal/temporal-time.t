use Test;

plan 12;

# times() returns ($user, $system) CPU times
{
    my ($user, $sys) = times;
    ok $user.defined, 'times returns defined user time';
    ok $sys.defined, 'times returns defined sys time';
    ok $user >= 0, 'user time is non-negative';
    ok $sys >= 0, 'sys time is non-negative';
}

# localtime() returns a formatted datetime string when called with no args
{
    my $str = localtime();
    ok $str.defined, 'localtime() returns a defined string';
    ok $str ~~ /\w+ \s+ \w+ \s+ \d/, 'localtime() string has expected format';
}

# localtime($epoch) returns a 9-element list
{
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(0);
    ok $sec.defined, 'localtime(0) returns defined seconds';
    ok $year == 1970, 'localtime(0) year is 1970 (or near it depending on timezone)' || $year == 1969;
}

# gmtime() returns a formatted datetime string when called with no args
{
    my $str = gmtime();
    ok $str.defined, 'gmtime() returns a defined string';
}

# gmtime($epoch) returns a 9-element list
{
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime(0);
    is $year, 1970, 'gmtime(0) year is 1970';
    is $mon, 0, 'gmtime(0) month is 0 (January)';
    is $mday, 1, 'gmtime(0) day is 1';
}
