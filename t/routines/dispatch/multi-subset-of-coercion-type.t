use v6;
use Test;

# Business::CreditCard: a parameter typed with a subset of a coercion type
# (`subset CC of Str() where ...`) is as narrow as the coercion parameter is,
# so it beats a plain `$` candidate for an argument that already is a Str.

plan 5;

subset CC of Str() where !*.contains(/ <-[\d x \ -]> /);
subset EE of Str();

multi a($, *% --> Int) { 0 }
multi a(CC $i, Str :$country = "US") { 1 }
multi c($) { 0 }
multi c(EE $i) { 1 }

is a("5212 3456"), 1, 'subset-of-coercion with where beats a bare $ (Str argument)';
is a("abc"), 0, 'the where clause still rejects';
is c("12"), 1, 'a bare subset of Str() beats $ for a Str';
sub t(CC $x) { "ok $x" }
is t("12"), 'ok 12', 'the subset binds';
is t(12), 'ok 12', 'a coercible argument binds';
