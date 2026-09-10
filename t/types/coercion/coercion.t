use Test;
plan 4;

is Int("42"), 42, 'Int(Str) coercion';
is Num("3.5"), 3.5, 'Num(Str) coercion';
is Str(123), "123", 'Str(Int) coercion';
is Bool(0), False, 'Bool(Int) coercion';
