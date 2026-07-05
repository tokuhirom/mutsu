use Test;

# Raku accepts only the exact tokens Inf/+Inf/-Inf/NaN when coercing a string
# to a number; the lower-case keywords inf/nan and the full word "Infinity" are
# NOT valid numeric strings. mutsu's .Num/.Numeric/.Real used Rust's lenient
# float parser, which accepted "inf"/"nan"/"Infinity" — inconsistent with the
# prefix + operator, which already rejected them.

plan 32;

# Valid non-finite tokens
is "Inf".Num, Inf, '"Inf".Num';
is "+Inf".Num, Inf, '"+Inf".Num';
is "-Inf".Num, -Inf, '"-Inf".Num';
ok "NaN".Num.isNaN, '"NaN".Num is NaN';
is "Inf".Numeric, Inf, '"Inf".Numeric';
is "Inf".Real, Inf, '"Inf".Real';

# Finite overflow still yields Inf (it is a valid numeric literal)
is "1e999".Num, Inf, '"1e999".Num overflows to Inf';
is "-1e999".Num, -Inf, '"-1e999".Num';

# Rust's lenient keywords are rejected, matching raku
throws-like { "inf".Num }, X::Str::Numeric, '"inf".Num throws';
throws-like { "nan".Num }, X::Str::Numeric, '"nan".Num throws';
throws-like { "Infinity".Num }, X::Str::Numeric, '"Infinity".Num throws';
throws-like { "+NaN".Num }, X::Str::Numeric, '"+NaN".Num throws';
throws-like { "-NaN".Num }, X::Str::Numeric, '"-NaN".Num throws';
throws-like { "inf".Numeric }, X::Str::Numeric, '"inf".Numeric throws';
throws-like { "inf".Real }, X::Str::Numeric, '"inf".Real throws';

# Ordinary numeric strings are unaffected
is "3.14".Num, 3.14e0, '"3.14".Num';
is "-5".Num, -5e0, '"-5".Num';
is "".Num, 0e0, 'empty string coerces to 0';

# --- .Num/.Real now use the canonical parser: radix prefixes and underscores
# work (previously only .Numeric/.Int/+ did) ---
is "1_000".Num, 1000e0, '.Num underscore separators';
is "0x1F".Num, 31e0, '.Num hex prefix';
is "0b101".Num, 5e0, '.Num binary prefix';
is "1_0.5_0".Num, 10.5e0, '.Num underscores in a decimal';
is "1_000".Real, 1000, '.Real underscore separators';
is "0x1F".Real, 31, '.Real hex prefix';
is "5".Real.WHAT.gist, "(Int)", '.Real of an integer string is Int';
is "3.14".Real.WHAT.gist, "(Rat)", '.Real of a decimal string is Rat';

# --- regressions caught by roast when .Num/.Real switched to the canonical
# parser (S32-num/stress.t, S32-num/negative-zero.t) ---

# A leading-dot mantissa is a valid numeric string in scientific notation too
# (the Rat path already accepted ".5").
is ".5e1".Num, 5e0, '.Num leading-dot mantissa with exponent';
is "-.5e1".Num, -5e0, '.Num signed leading-dot mantissa';
is ".5".Num, 0.5e0, '.Num leading-dot decimal';
is ".5e1".Numeric, 5e0, '.Numeric leading-dot mantissa with exponent';

# "-0" (Int-shaped zero) must still coerce to the IEEE negative zero as a Num;
# atan2 distinguishes the two zero signs.
is atan2("-0".Num, -1e0), atan2(-0e0, -1e0), '"-0".Num keeps the negative zero';
is atan2("-0.0".Num, -1e0), atan2(-0e0, -1e0), '"-0.0".Num keeps the negative zero';
