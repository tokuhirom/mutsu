use v6;
use Test;

# `Date` and `DateTime` compose the `Dateish` role in rakudo, but mutsu's
# built-in type catalog did not record it, so `.^roles` answered `()` and
# `.^mro(:roles)` skipped it -- which made the `are` distribution infer `Any`
# instead of `Dateish` as the common type of a Date and a DateTime (#9347).

plan 7;

is-deeply DateTime.^roles, (Dateish,), 'DateTime.^roles';
is-deeply Date.^roles, (Dateish,), 'Date.^roles';
is-deeply DateTime.^mro(:roles), (DateTime, Dateish, Any, Mu), 'DateTime.^mro(:roles)';
is-deeply Date.^mro(:roles), (Date, Dateish, Any, Mu), 'Date.^mro(:roles)';
is-deeply (DateTime.now, Date.today).are, Dateish, '.are finds the shared role';
is-deeply Promise.^mro(:roles), (Promise, Awaitable, Any, Mu), 'Promise.^mro(:roles) reads the catalog too';
is-deeply (1, 2, Mu.new).are, Mu, 'an instance of Mu keeps its own type as a candidate';
