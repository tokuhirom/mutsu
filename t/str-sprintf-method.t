# `$format.sprintf(*@args)` — the method form of `sprintf`, equivalent to
# `sprintf($format, @args)` (raku-doc Type/independent-routines: "%s's weight is
# %.2f %s".sprintf(...)). mutsu had only the 0-arg passthrough and a 1-arg native
# fast path; two-or-more args threw X::Method::NotFound.
use Test;

plan 9;

is "%s=%s".sprintf("k", "v"), 'k=v', 'two string args';
is "%s the %d%s".sprintf("þor", 1, "st"), 'þor the 1st', 'three mixed args';
is "%05d".sprintf(42), '00042', 'single numeric arg still works';
is "%d + %d = %d".sprintf(2, 3, 5), '2 + 3 = 5', 'three numeric args';
is "%.2f".sprintf(3.14159), '3.14', 'float precision, single arg';
is "%x-%o-%b".sprintf(255, 8, 5), 'ff-10-101', 'hex/oct/bin radix args';
is "%-5s|".sprintf("hi"), 'hi   |', 'left-justify width';
is "no directives".sprintf(), 'no directives', 'zero args, no directives';
is "%2\$s %1\$s".sprintf("a", "b"), 'b a', 'positional argument indices';
