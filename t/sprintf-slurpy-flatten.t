use v6;
use Test;

# `sprintf($format, *@args)` slurps its arguments, so every list-like argument
# (Range, List/Seq/Slip, bare Array literal) flattens into one flat positional
# list before the directives consume them. This mirrors raku exactly and covers
# both the sub form and the `$format.sprintf(...)` method form.

# --- Range arguments flatten (was: "1 argument was supplied" arg-count error) ---
is sprintf("%d %d", 1..2), "1 2", "sub form: single Range flattens to 2 args";
is sprintf("%d %d %d", 1..3), "1 2 3", "sub form: single Range flattens to 3 args";
is sprintf("%d %d %d", 1, 2..3), "1 2 3", "sub form: scalar + Range flatten together";
is sprintf("%d %d %d %d", 1..2, 3..4), "1 2 3 4", "sub form: two Ranges flatten";

# --- List / nested list arguments flatten recursively ---
is sprintf("%d %d %d", (1, 2, 3)), "1 2 3", "sub form: flat list flattens";
is sprintf("%d %d %d", (1, (2, 3))), "1 2 3", "sub form: nested list flattens recursively";

# --- Seq / Slip arguments flatten ---
is sprintf("%d %d", (1, 2).Seq), "1 2", "sub form: Seq flattens";
is sprintf("%d %d %d", |(1, 2, 3)), "1 2 3", "sub form: Slip flattens";

# --- Array arguments flatten (regression guard) ---
{
    my @a = 1..3;
    is sprintf("%d %d %d", @a), "1 2 3", "sub form: named array flattens";
}

# --- Itemization is respected: a scalar-held array stays one argument ---
{
    my $x = [1, 2, 3];
    is sprintf("%s", $x), "1 2 3", "scalar-held array is a single (unflattened) arg";
}

# --- Method form: same slurpy flatten ---
is "%d %d".sprintf(1..2), "1 2", "method form: single Range flattens";
is "%d %d %d".sprintf(1..3), "1 2 3", "method form: 3-element Range flattens";
is "%d %d %d".sprintf(1, 2..3), "1 2 3", "method form: scalar + Range flatten";
is "%s-%s".sprintf([1, 2]), "1-2", "method form: single Array spreads across directives";
is "%d %d".sprintf((1, 2).Seq), "1 2", "method form: Seq flattens";

# --- Plain scalar args still work (fast-path regression guard) ---
is sprintf("%d", 42), "42", "sub form: plain scalar";
is "%d".sprintf(42), "42", "method form: plain scalar";
is sprintf("%s %s", "a", "b"), "a b", "sub form: two string scalars";

# --- printf writes the same flattened output ---
lives-ok { printf("%d %d\n", 1..2) }, "printf accepts a Range and flattens it";

done-testing;
