use v6;
use Test;

# The `$format.sprintf(...)` method form must validate the directive count
# against the number of arguments, exactly as the `sprintf($format, ...)` sub
# form does. Previously mutsu's method-form fast path silently rendered missing
# directives as empty/0 (`"%s and %s".sprintf("a")` gave "a and " instead of
# throwing X::Str::Sprintf::Directives::Count).

# --- Too few args: single-arg method form errors like the sub form ---
throws-like { "%s and %s".sprintf("a") }, X::Str::Sprintf::Directives::Count,
    "method form: 2 directives, 1 arg throws Count";
throws-like { "%d-%d-%d".sprintf(1) }, X::Str::Sprintf::Directives::Count,
    "method form: 3 directives, 1 arg throws Count";
throws-like { "%d %d".sprintf(5) }, X::Str::Sprintf::Directives::Count,
    "method form: 2 directives, 1 arg throws Count";

# --- Too few args: no-arg method form errors too ---
throws-like { "%s".sprintf }, X::Str::Sprintf::Directives::Count,
    "no-arg method form: 1 directive, 0 args throws Count";
throws-like { "%d %d".sprintf() }, X::Str::Sprintf::Directives::Count,
    "no-arg method form: 2 directives, 0 args throws Count";

# --- The sub form throws the same exception (baseline) ---
throws-like { sprintf("%s and %s", "a") }, X::Str::Sprintf::Directives::Count,
    "sub form: 2 directives, 1 arg throws Count";

# --- Correct arg counts still render (fast-path regression guard) ---
is "%s".sprintf("x"), "x", "method form: 1 directive, 1 arg";
is "%d".sprintf(42), "42", "method form: numeric directive";
is "%05.2f".sprintf(3.14), "03.14", "method form: width/precision";
is "%x".sprintf(255), "ff", "method form: hex";
is "%d %d".sprintf(1, 2), "1 2", "method form: 2 directives, 2 args";
is "%2\$s %1\$s".sprintf("a", "b"), "b a", "method form: positional args";
is "%*d".sprintf(5, 42), "   42", "method form: '*' width consumes an arg";

# --- Directive-free formats render as themselves with no args ---
is "no directives".sprintf, "no directives", "no-arg method form: literal string";
is 42.sprintf, "42", "no-arg method form: numeric invocant stringifies";
is 3.14.sprintf, "3.14", "no-arg method form: rational invocant";

# --- An escaped %% collapses to a literal % even with no args ---
is "100%%".sprintf, "100%", "no-arg method form: %% collapses to %";
is "50%% off".sprintf, "50% off", "no-arg method form: %% mid-string";

done-testing;
