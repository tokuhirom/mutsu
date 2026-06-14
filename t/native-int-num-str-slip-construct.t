use v6;
use Test;

# `Int.new(v)`, `Num.new(v)`, `Str.new` and `Slip.new(...)` are pure data
# constructors — yet `.new` routed through the interpreter's generic
# `dispatch_new`. They now build through the shared `try_native_builtin_construct`
# entry (Int/Num via `build_native_{int,num}_value`, Str/Slip inline) that the
# interpreter arms also use, keeping them byte-identical. `Bool` is deliberately
# left on the interpreter (it is an enum, so `Bool.new` errors before the
# basic-type arm — the native path preserves that by falling through).

plan 13;

# --- Int ---
is Int.new(5), 5, 'Int.new(5)';
is Int.new, 0, 'Int.new defaults to 0';
is Int.new(3.9), 3, 'Int.new truncates a Num';
is Int.new(-7), -7, 'Int.new negative';
dies-ok { Int.new(Int) }, 'Int.new on a type object dies';

# --- Num ---
is Num.new(1.5), 1.5e0, 'Num.new(1.5)';
is Num.new, 0e0, 'Num.new defaults to 0e0';
is Num.new(3), 3e0, 'Num.new from an Int';
dies-ok { Num.new(Int) }, 'Num.new on a type object dies';

# --- Str ---
is Str.new, '', 'Str.new is the empty string';

# --- Slip ---
is Slip.new(1, 2, 3).elems, 3, 'Slip.new from elements';
is (Slip.new(1, 2), 3).elems, 3, 'a Slip flattens into a surrounding list';

# --- arithmetic on a constructed Int ---
is (Int.new(10) + 5), 15, 'arithmetic on a constructed Int';
