use Test;

# TimeBomb 0.0.1 uses Str smartmatch to verify the Date stored in its
# X::TimeBomb::Expired exception. Str.ACCEPTS(Date/DateTime) compares their
# string representations.
plan 4;

ok Date.new('0001-01-01') ~~ '0001-01-01',
    'Date smartmatches its ISO string';
nok Date.new('0001-01-01') ~~ '0001-01-02',
    'Date does not smartmatch a different ISO string';
ok DateTime.new(1, 1, 1, 0, 0, 0) ~~ '0001-01-01T00:00:00Z',
    'DateTime smartmatches its ISO string';
nok DateTime.new(1, 1, 1, 0, 0, 0) ~~ '0001-01-01',
    'DateTime does not smartmatch a date-only string';
