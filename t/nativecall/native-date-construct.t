use v6;
use Test;

# `Date.new(...)` is pure data — parse named/positional args (a date string, a
# DateTime/Instant to take the date of, or y/m/d), validate, and build the Date
# — except a `:formatter`, which renders a user Callable. It now constructs
# through the shared `try_native_builtin_construct` entry via
# `build_native_date`, so the common (no-formatter) case is VM-native; a
# `:formatter` falls through to the interpreter (the only self-dependent step).
# The interpreter arm calls the same helper, keeping them byte-identical.

plan 14;

is Date.new(2020, 1, 1).Str, '2020-01-01', 'Date.new(y, m, d)';
is Date.new(2020, 2, 29).Str, '2020-02-29', 'leap day is valid';
is Date.new("2021-06-15").Str, '2021-06-15', 'Date.new from a string';
is Date.new(year => 2000, month => 12, day => 25).Str, '2000-12-25',
    'Date.new from named args';

my $d = Date.new(2020, 3, 15);
is $d.year, 2020, '.year';
is $d.month, 3, '.month';
is $d.day, 15, '.day';
is $d.WHAT.^name, 'Date', 'WHAT is Date';

# --- from a DateTime ---
is Date.new(DateTime.new(2020, 5, 5, 10, 30, 0)).Str, '2020-05-05',
    'Date.new from a DateTime takes its date';

# --- validation errors ---
dies-ok { Date.new(2020, 13, 1) }, 'invalid month dies';
dies-ok { Date.new(2021, 2, 29) }, 'invalid leap day dies';
dies-ok { Date.new }, 'Date.new with no args dies';

# --- day-of-week is computed ---
is Date.new(2020, 1, 1).day-of-week, 3, '2020-01-01 is a Wednesday';

# --- a :formatter still works (falls through to the interpreter) ---
is Date.new(2020, 1, 1, formatter => { sprintf "%04d!", .year }).Str, '2020!',
    'a :formatter renders via the interpreter';
