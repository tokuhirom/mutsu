use v6;
use Test;

# `Date` and `DateTime` compose the `Dateish` role in rakudo, but mutsu's
# built-in role seeds did not list it, so `.^roles` answered `()` and
# `.^mro(:roles)` skipped it -- which made the `are` distribution infer `Any`
# instead of `Dateish` as the common type of a Date and a DateTime (#9347).

plan 5;

is-deeply DateTime.^roles, (Dateish,), 'DateTime.^roles';
is-deeply Date.^roles, (Dateish,), 'Date.^roles';
is-deeply DateTime.^mro(:roles), (DateTime, Dateish, Any, Mu), 'DateTime.^mro(:roles)';
is-deeply Date.^mro(:roles), (Date, Dateish, Any, Mu), 'Date.^mro(:roles)';
is-deeply (DateTime.now, Date.today).are, Dateish, '.are finds the shared role';
