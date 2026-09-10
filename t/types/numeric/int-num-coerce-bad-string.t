use Test;

# The `Int(...)` / `Num(...)` coercion *function* forms must behave like the
# `.Int` / `.Num` *method* forms: an invalid string yields a lazy X::Str::Numeric
# Failure, not a silent 0.

plan 18;

# --- Valid strings still coerce ---
is Int("42"), 42, 'Int("42") coerces';
is Int("  42  "), 42, 'Int trims surrounding whitespace';
is Int("0xff"), 255, 'Int parses a radix prefix';
is Num("3.14"), 3.14, 'Num("3.14") coerces';
is Num("-2.5"), -2.5, 'Num parses a signed decimal';
is Num("1e3"), 1000e0, 'Num parses scientific notation';

# --- Invalid strings produce an X::Str::Numeric Failure (function form) ---
{
    my $r = try Int("abc");
    ok !$r.defined, 'Int("abc") is an undefined (failed) value';
    is $!.WHAT.^name, 'X::Str::Numeric', 'Int("abc") sets $! to X::Str::Numeric';
}
{
    my $r = try Num("xyz");
    ok !$r.defined, 'Num("xyz") is an undefined (failed) value';
    is $!.WHAT.^name, 'X::Str::Numeric', 'Num("xyz") sets $! to X::Str::Numeric';
}
{
    my $r = try Int("12abc");
    ok !$r.defined, 'Int("12abc") (trailing chars) fails';
    is $!.WHAT.^name, 'X::Str::Numeric', 'trailing-char string is X::Str::Numeric';
}

# --- Function form matches method form ---
{
    my $r = try "abc".Int;
    is $!.WHAT.^name, 'X::Str::Numeric', 'method form .Int agrees';
}
{
    my $r = try "xyz".Num;
    is $!.WHAT.^name, 'X::Str::Numeric', 'method form .Num agrees';
}

# --- Defaulting past the Failure works ---
is (try Int("abc")) // 99, 99, 'a failed Int defaults via //';
is (try Num("xyz")) // 99, 99, 'a failed Num defaults via //';

# --- Non-string coercions are unchanged ---
is Int(3.9), 3, 'Int truncates a Num';
is Num(5), 5e0, 'Num of an Int';
