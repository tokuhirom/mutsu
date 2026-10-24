use v6;
use Test;

plan 9;

# rakudo leaves a DateTime alone for a time unit it does not know, rather than
# raising: Red's tests write `DateTime.now.later(:1sec)`.
my $d = DateTime.new(2020, 1, 1, 0, 0, 0);

is $d.later(:sec(5)).Str, "2020-01-01T00:00:00Z", ":sec is not a unit and changes nothing";
is $d.later(:secs(5)).Str, "2020-01-01T00:00:00Z", ":secs likewise";
is $d.later(:min(5)).Str, "2020-01-01T00:00:00Z", ":min likewise";
is $d.later(:frobs(2)).Str, "2020-01-01T00:00:00Z", "an invented unit likewise";
is $d.earlier(:sec(5)).Str, "2020-01-01T00:00:00Z", "earlier ignores it the same way";

is $d.later(:seconds(5)).Str, "2020-01-01T00:00:05Z", ":seconds still applies";
is $d.later(:second(5)).Str, "2020-01-01T00:00:05Z", ":second still applies";
is $d.later(:minute(5)).Str, "2020-01-01T00:05:00Z", ":minute still applies";
is $d.later(:day(5)).Str, "2020-01-06T00:00:00Z", ":day still applies";
