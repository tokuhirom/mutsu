use v6;
use Test;

# `Instant.from-posix(secs)` is a pure *class* method (a type-object method
# other than `.new`): it maps a POSIX timestamp to TAI seconds and wraps it in
# an `Instant`. It now dispatches through the new VM-native class-method hook
# `try_native_builtin_class_method` instead of routing through the interpreter's
# generic class-method dispatch. The interpreter calls the same arm, so the two
# are byte-identical. (This hook is the scaffold for native-izing further pure
# built-in class methods.) `.to-posix` returns `(posix, leap-second-flag)`.

plan 7;

is Instant.from-posix(0).to-posix[0], 0, 'epoch instant round-trips to posix 0';
is Instant.from-posix(1000000000).to-posix[0], 1000000000,
    'a large posix timestamp round-trips';
is Instant.from-posix(1.5).to-posix[0], 1.5, 'fractional seconds preserved';
is Instant.from-posix(1000).WHAT.^name, 'Instant', 'produces an Instant';

# --- arithmetic between instants yields the elapsed seconds ---
is (Instant.from-posix(100) - Instant.from-posix(40)), 60,
    'difference of two instants is the elapsed seconds';

# --- conversion to DateTime ---
is Instant.from-posix(1600000000).DateTime.year, 2020,
    'Instant -> DateTime carries the right year';

# --- negative posix (before epoch) ---
is Instant.from-posix(-86400).to-posix[0], -86400, 'a pre-epoch instant';
