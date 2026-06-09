use Test;

plan 6;

# DateTime.new / Date.new with a malformed string raise
# X::Temporal::InvalidFormat carrying `invalid-str` and `target`.

throws-like 'DateTime.new("2012/04")', X::Temporal::InvalidFormat,
    invalid-str => '2012/04', target => 'DateTime';

throws-like 'Date.new("2012/04")', X::Temporal::InvalidFormat,
    invalid-str => '2012/04', target => 'Date';

throws-like 'DateTime.new("2012/04")', X::Temporal::InvalidFormat,
    message => /:s Invalid DateTime string/;

throws-like 'Date.new("2012/04")', X::Temporal::InvalidFormat,
    message => /:s Invalid Date string/;

# Valid constructions still work.
is DateTime.new("2012-04-01T00:00:00Z").Str, '2012-04-01T00:00:00Z',
    'valid DateTime parses';
is Date.new("2012-04-01").Str, '2012-04-01', 'valid Date parses';
