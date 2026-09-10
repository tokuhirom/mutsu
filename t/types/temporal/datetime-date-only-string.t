use Test;

plan 5;

# `DateTime.new(Str)` accepts a date-only string (no time part): the time
# defaults to midnight UTC. Previously mutsu required a `T` separator and died
# with "Invalid DateTime string".
is DateTime.new("2023-03-04").Str, '2023-03-04T00:00:00Z',
    'a date-only string defaults to midnight UTC';

is DateTime.new("2012-04-01").Str, '2012-04-01T00:00:00Z',
    'another date-only string';

# A full datetime string still parses.
is DateTime.new("2023-03-04T05:06:07Z").Str, '2023-03-04T05:06:07Z',
    'a full datetime string still parses';

# A malformed string still raises the DateTime-flavored error.
throws-like 'DateTime.new("2012/04")', X::Temporal::InvalidFormat,
    target => 'DateTime';

throws-like 'DateTime.new("garbage")', X::Temporal::InvalidFormat,
    target => 'DateTime';
