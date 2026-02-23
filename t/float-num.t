use Test;
plan 8;

is 3.14.WHAT, "(Rat)", "decimal literal type is Rat";
ok 1.5 + 2.5 == 4, "float addition";
ok 3.0 * 2.0 == 6, "float multiplication";
ok 1.5 < 2.5, "float less than";
ok 2.5 > 1.5, "float greater than";
is (-3.5).abs, 3.5, "float abs";
is (-42).abs, 42, "int abs";
ok 1e3 == 1000, "scientific notation";
