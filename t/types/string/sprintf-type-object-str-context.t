use v6;
use Test;

# A bare type object rendered by `%s` (string context) stringifies to the empty
# string with rakudo's "Use of uninitialized value of type X in string context"
# warning, instead of the `(Int)` gist. Numeric directives keep coercing a type
# object to 0. Both the sub form `sprintf(...)` and the method form
# `$fmt.sprintf(...)` must agree.

plan 12;

# --- value coercion (warnings are quieted; checked separately below) ---
is quietly(sprintf("%s", Int)),        "",     'sub form: %s on Int -> ""';
is quietly(sprintf("%s", Str)),        "",     'sub form: %s on Str -> ""';
is quietly(sprintf("%s|%s", Int, 5)),  "|5",   'sub form: mixed type-object / value';
is quietly("%s".sprintf(Int)),         "",     'method form: %s on Int -> ""';
is quietly("%s %s".sprintf(Int, 5)),   " 5",   'method form: mixed';
is quietly(sprintf("%-5s|", Int)),     "     |", 'width padding applies to the "" coercion';
is quietly(sprintf("%.2s", Str)),      "",     'precision truncation of ""';

# Concrete values are unaffected.
is sprintf("%s", 42),   "42", 'concrete value still renders';
is "%d".sprintf(42),    "42", 'method form concrete value still renders';

# A class defining `.Str` dispatches even for the type object (no warning).
class HasStr { method Str { "custom" } }
is sprintf("%s", HasStr),  "custom", 'user .Str on a type object dispatches (sub form)';
is "%s".sprintf(HasStr),   "custom", 'user .Str on a type object dispatches (method form)';

# The string-context warning is actually emitted.
{
    my $warned = False;
    my $r;
    {
        CONTROL { when CX::Warn { $warned = True; .resume } }
        $r = sprintf("%s", Int);
    }
    ok $warned && $r eq "", 'warning is emitted for %s on a type object';
}
