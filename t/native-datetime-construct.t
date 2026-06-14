use v6;
use Test;

# `DateTime.new(...)` parses named/positional args (y..s, :timezone, :date, an
# ISO-8601 string, a posix epoch, a Date/Instant), validates, and builds the
# DateTime — pure `temporal::*` logic — except a `:formatter`, which renders a
# user Callable. It now constructs through the shared `try_native_builtin_
# construct` entry via `build_native_datetime`, so the common (no-formatter)
# case is VM-native; a `:formatter` falls through to the interpreter. The
# interpreter arm calls the same helper, keeping them byte-identical.

plan 14;

is DateTime.new(2020, 1, 1, 10, 30, 0).Str, '2020-01-01T10:30:00Z',
    'DateTime.new(y, mo, d, h, mi, s)';
is DateTime.new(2020, 2, 29, 23, 59, 59).Str, '2020-02-29T23:59:59Z',
    'leap-day datetime';
is DateTime.new("2021-06-15T08:30:00Z").Str, '2021-06-15T08:30:00Z',
    'DateTime.new from an ISO-8601 string';
is DateTime.new(year => 2000, month => 12, day => 25, hour => 6).Str,
    '2000-12-25T06:00:00Z', 'DateTime.new from named component args';

my $dt = DateTime.new(2020, 3, 15, 9, 45, 30);
is $dt.year, 2020, '.year';
is $dt.hour, 9, '.hour';
is $dt.minute, 45, '.minute';
is $dt.WHAT.^name, 'DateTime', 'WHAT is DateTime';

# --- from a :date named arg ---
is DateTime.new(date => Date.new(2020, 5, 5)).Str, '2020-05-05T00:00:00Z',
    'DateTime.new(:date) takes the date at midnight';

# --- posix epoch ---
is DateTime.new(0).Str, '1970-01-01T00:00:00Z', 'DateTime.new(0) is the epoch';

# --- timezone ---
is DateTime.new(2020, 1, 1, 0, 0, 0, timezone => 3600).timezone, 3600,
    'timezone is preserved';

# --- validation errors ---
dies-ok { DateTime.new(2020, 13, 1, 0, 0, 0) }, 'invalid month dies';
dies-ok { DateTime.new }, 'DateTime.new with no args dies';

# --- a :formatter constructs (rendering falls through to the interpreter) ---
ok DateTime.new(2020, 1, 1, 12, 0, 0,
        formatter => { sprintf "%02d", .hour }) ~~ DateTime,
    'a :formatter still constructs a DateTime';
