use Test;

# `$?LINE` and `$?TABSTOP` are compile-time constants that must be substituted
# at their source position even inside string interpolation. Previously the
# interpolated form produced a runtime `Var("?LINE")` that resolved to the empty
# string (`"line $?LINE"` -> `"line "`).

plan 6;

# Interpolated `$?LINE` equals the bare `$?LINE` on the same physical line.
is "$?LINE".Int, $?LINE, 'bare $?LINE interpolation gives the source line';
is "L=$?LINE", "L={$?LINE}", '$?LINE interpolation with surrounding text';

# Two references on consecutive lines differ by exactly one.
my $a = "$?LINE".Int;
my $b = "$?LINE".Int;
is $b - $a, 1, 'consecutive-line references differ by one';
ok $a > 0, 'interpolated $?LINE is a positive line number';

# A previously-empty result is now non-empty.
isnt "$?LINE", '', '$?LINE interpolation is no longer the empty string';

is "tab=$?TABSTOP", "tab=8", '$?TABSTOP interpolates to 8';
