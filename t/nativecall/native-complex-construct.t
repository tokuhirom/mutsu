use v6;
use Test;

# `Complex.new(re, im)` is pure data — it builds a `Value::Complex` with each
# component coerced to f64 — yet `.new` routed through the interpreter's generic
# `dispatch_new`. It now constructs through the shared
# `try_native_builtin_construct` entry (joining Buf/utf8/Uni/Version/Duration/…),
# so the VM builds it directly. The interpreter arm calls the same
# `build_native_complex_value` helper, keeping the two byte-identical.

plan 12;

is Complex.new(3, 4).Str, '3+4i', 'Complex.new(re, im)';
is Complex.new(3, 4).re, 3, 'real part';
is Complex.new(3, 4).im, 4, 'imaginary part';
is Complex.new(3, 4).reals, (3, 4), 'reals list';
ok Complex.new(3, 4) ~~ Complex, 'is a Complex';
is Complex.new(3, 4).WHAT.^name, 'Complex', 'WHAT is Complex';

# --- component coercions ---
is Complex.new(1.5, 2.5).Str, '1.5+2.5i', 'Num components';
is Complex.new(1/2, 3).Str, '0.5+3i', 'Rat component coerces to f64';

# --- defaults (mutsu is more lenient than raku here; matches the interpreter) ---
is Complex.new.Str, '0+0i', 'Complex.new with no args is 0+0i';
is Complex.new(3).Str, '3+0i', 'Complex.new with one arg defaults im to 0';
is Complex.new(0, 0).Str, '0+0i', 'zero Complex';

# --- arithmetic on a freshly constructed Complex ---
is (Complex.new(1, 2) + Complex.new(3, 4)).Str, '4+6i',
    'arithmetic on constructed Complex values';
