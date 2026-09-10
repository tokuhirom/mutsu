use v6;
use Test;

# `Rat.new(n, d)`, `FatRat.new(n, d)` and `Pair.new(...)` are pure data
# constructors — yet `.new` routed through the interpreter's generic
# `dispatch_new` on every call. They now build through the shared
# `try_native_builtin_construct` entry via `build_native_{rat,fatrat,pair}_value`
# helpers that the interpreter arms also call, keeping the two byte-identical.

plan 16;

# --- Rat ---
is Rat.new(1, 2), 0.5, 'Rat.new(1, 2)';
is Rat.new(3, 4).nude, (3, 4), 'Rat.new keeps numerator/denominator';
is Rat.new(6, 4).nude, (3, 2), 'Rat.new normalizes';
is Rat.new(5).nude, (5, 1), 'Rat.new(n) defaults denominator to 1';
is Rat.new.nude, (0, 1), 'Rat.new with no args is 0/1';
is Rat.new(10000000000000000000, 3).nude,
    (10000000000000000000, 3), 'Rat.new with a BigInt numerator';

# --- FatRat ---
is FatRat.new(1, 3).WHAT.^name, 'FatRat', 'FatRat.new produces a FatRat';
is FatRat.new(2, 4).nude, (1, 2), 'FatRat.new normalizes';

# --- Pair (positional) ---
is Pair.new("a", 1).key, 'a', 'Pair.new positional key';
is Pair.new("a", 1).value, 1, 'Pair.new positional value';
is Pair.new("a", 1).gist, 'a => 1', 'Pair.new gist';

# --- Pair (named) ---
is Pair.new(key => "k", value => 42).key, 'k', 'Pair.new(:key) key';
is Pair.new(key => "k", value => 42).value, 42, 'Pair.new(:value) value';

# --- a non-string key yields a ValuePair-style pair ---
is Pair.new(5, "v").key, 5, 'Pair.new with a numeric key';
is Pair.new(5, "v").value, 'v', 'Pair.new numeric-key value';

# --- arithmetic on constructed Rats ---
is (Rat.new(1, 2) + Rat.new(1, 3)), Rat.new(5, 6), 'arithmetic on constructed Rats';
