use Test;

# `(&f = SUB)(ARGS)` assigns the routine variable, then calls the assigned
# sub: a call whose invocant is a parenthesised assignment to a `&` variable.
# Needle::Compile's self-replacing stub is written this way (#15164).

plan 5;

my &jp = my sub jp-stub(str $pattern) {
    (&jp = anon sub jp(str $pattern) { "real:$pattern" })($pattern)
}
is jp("x"), "real:x", 'the first call replaces &jp and calls the new sub';
is jp("y"), "real:y", 'later calls run the replacement directly';

my &g;
is (&g = sub ($n) { $n * 2 })(21), 42, 'a parenthesised &g = SUB called at once';
is g(4), 8, 'and &g stays assigned';

my &h = -> $x { $x };
is (&h = sub ($x) { $x + 1 })(1), 2, 'reassigning an already assigned &h';
