use Test;

plan 6;

# `Date.new(year, month, *)` — a Whatever day means the last day of that month.
is Date.new(2042, 2, *).Str, '2042-02-28', 'Whatever day -> last day (non-leap Feb)';
is Date.new(2044, 2, *).Str, '2044-02-29', 'Whatever day -> last day (leap Feb)';
is Date.new(2043, 4, *).Str, '2043-04-30', 'Whatever day -> last day (April)';
is Date.new(2044, 2, 15).Str, '2044-02-15', 'an explicit day is unaffected';

# `Date.new-from-daycount(N)` — inverse of `.daycount` (Modified Julian Day).
is Date.new-from-daycount(49987).Str, '1995-09-27', 'new-from-daycount reconstructs the date';
is Date.new(1995, 9, 27).daycount, 49987, 'daycount round-trips';
