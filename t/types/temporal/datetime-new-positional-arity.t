use Test;

# #18944: DateTime.new's positional candidates take one argument (epoch,
# Instant, Str, Date) or six ($y, $mo, $d, $h, $mi, $s). Any other positional
# count is X::Multi::NoMatch, not an epoch read from the first argument.

plan 10;

for 2..5 -> $n {
    throws-like { DateTime.new(|(1 xx $n)) }, X::Multi::NoMatch,
        "$n positionals match no candidate";
}
throws-like { DateTime.new(1, 2, 3, 4, 5, 6, 7) }, X::Multi::NoMatch,
    'seven positionals match no candidate';
throws-like { DateTime.new(2024, 1, 2, :timezone(3600)) }, X::Multi::NoMatch,
    'a named argument does not rescue three positionals';

is DateTime.new(2024, 1, 2, 3, 4, 5).Str, '2024-01-02T03:04:05Z', 'six positionals';
is DateTime.new(0).Str, '1970-01-01T00:00:00Z', 'one positional is an epoch';

class D is DateTime { }
throws-like { D.new(2024, 1, 2) }, X::Multi::NoMatch,
    'a subclass refuses three positionals too';
is D.new(2024, 1, 2, 3, 4, 5).hour, 3, 'a subclass takes six positionals';
