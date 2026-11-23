use Test;

# rakudo's X::Temporal::InvalidFormat builds its message from its attributes:
# "Invalid <target> string '<invalid-str>'; use <format> instead". A string
# that is no timestamp at all is refused as a DateTime string by both
# `DateTime.new` and `Str.DateTime`.

plan 6;

my $dt-format = 'an ISO 8601 timestamp (yyyy-mm-ddThh:mm:ssZ or yyyy-mm-ddThh:mm:ss+01:00)';

throws-like { DateTime.new("garbage") }, X::Temporal::InvalidFormat,
    message => "Invalid DateTime string 'garbage'; use $dt-format instead",
    'DateTime.new';
throws-like { "garbage".DateTime }, X::Temporal::InvalidFormat,
    message => "Invalid DateTime string 'garbage'; use $dt-format instead",
    'Str.DateTime';
throws-like { Date.new("garbage") }, X::Temporal::InvalidFormat,
    message => "Invalid Date string 'garbage'; use yyyy-mm-dd instead",
    'Date.new';
try DateTime.new("garbage");
is $!.format, $dt-format, '.format';
is $!.target, 'DateTime', '.target';
is "2024-01-02".DateTime.Str, '2024-01-02T00:00:00Z', 'a date-only string is midnight UTC';
