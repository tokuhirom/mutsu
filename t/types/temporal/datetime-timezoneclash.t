use Test;

plan 6;

# A numeric timestamp offset in the string combined with an explicit
# :timezone argument raises X::DateTime::TimezoneClash. A bare `Z` is exempt.

throws-like 'DateTime.new("1998-12-31T23:59:60+0200", :timezone<Z>)',
    X::DateTime::TimezoneClash;

throws-like 'DateTime.new("2024-01-01T00:00:00+0000", :timezone(3600))',
    X::DateTime::TimezoneClash;

throws-like 'DateTime.new("2024-01-01T00:00:00+0200", :timezone(0))',
    X::DateTime::TimezoneClash, message => /:s not allowed with a timestamp offset/;

# `Z` (UTC designator) is not a numeric offset, so :timezone is allowed.
lives-ok { EVAL 'DateTime.new("2024-01-01T00:00:00Z", :timezone(3600))' },
    'Z designator allows :timezone';

# No offset in the string + :timezone is fine.
lives-ok { EVAL 'DateTime.new("2024-01-01T00:00:00", :timezone(3600))' },
    'no offset allows :timezone';

# An offset in the string without :timezone is fine.
is DateTime.new("2024-01-01T00:00:00+0200").Str, '2024-01-01T00:00:00+02:00',
    'offset string without :timezone parses';
