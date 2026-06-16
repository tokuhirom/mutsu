use Test;

plan 18;

# The single-argument rule slurpy `+@a` / `+foo`:
#   - a single Iterable argument is used as the argument list (flattened),
#   - multiple top-level arguments are NOT flattened (each becomes one element).

# --- +@foo (array sigil) ---
sub a(+@foo) { @foo }

is a(1,2,3).elems,        3, '+@: comma separates top-level args';
is a((1,2,3)).elems,      3, '+@: single list arg flattens';
is a([1,2,3]).elems,      3, '+@: single array arg flattens';
is a(1..3).elems,         3, '+@: single range arg flattens';
is a(1...3).elems,        3, '+@: single seq arg flattens';
is a((1,2,3),4).elems,    2, '+@: list + extra arg is NOT flattened';
is a([1,2,3],4).elems,    2, '+@: array + extra arg is NOT flattened';
is a(1..3,4).elems,       2, '+@: range + extra arg is NOT flattened';

# --- +foo (sigilless) ---
sub s(+foo) { foo }

is s(1,2,3).elems,        3, '+foo: comma separates top-level args';
is s((1,2,3)).elems,      3, '+foo: single list arg flattens';
is s(1..3).elems,         3, '+foo: single range arg flattens';
is s(1..3,4).elems,       2, '+foo: range + extra arg is NOT flattened';
is s([1,2,3],4).elems,    2, '+foo: array + extra arg is NOT flattened';

# --- with a leading positional / named ---
sub f($x, :$y, +@foo) { @foo }

is f(0,:y,1,2,3).elems,   3, '+@ after positional: comma separates';
is f(0,:y,1..3).elems,    3, '+@ after positional: single range flattens';
is f(0,:y,1..3,4).elems,  2, '+@ after positional: range + extra not flattened';

# slips always flatten, even among multiple args
is a(|(1,2),3).elems,     3, '+@: a slip flattens among multiple args';
is a(slip(1,2),3,4).elems, 4, '+@: slip + extras';
