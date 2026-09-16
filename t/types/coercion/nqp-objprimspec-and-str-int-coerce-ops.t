use v6;
use Test;
use nqp;

# Four `nqp::` op gaps found via the ecosystem sweep's `blocked_load`
# records (agent task, 2026-09-16): every one of the affected distributions
# failed at `use` time with "Unsupported nqp:: op: nqp::<op>", before any of
# their own tests could run.
#  - `nqp::objprimspec($type)` — AttrX::Mooish (Async::Workers, Cooklang)
#  - `nqp::coerce_is`/`nqp::coerce_si` and `nqp::unbox_s` — Net::Netmask::Fast

plan 15;

# --- nqp::objprimspec ------------------------------------------------------
# 0 = boxed/object type, 1 = signed int family, 10 = unsigned int family,
# 2 = num, 3 = str (verified against `raku -e 'nqp::objprimspec(...)'`; the
# unsigned family answers 10 rather than folding into 1).
is nqp::objprimspec(int), 1, 'objprimspec(int) is 1';
is nqp::objprimspec(int32), 1, 'objprimspec(int32) is 1';
is nqp::objprimspec(uint32), 10, 'objprimspec(uint32) is 10';
is nqp::objprimspec(num), 2, 'objprimspec(num) is 2';
is nqp::objprimspec(str), 3, 'objprimspec(str) is 3';
is nqp::objprimspec(Int), 0, 'objprimspec(Int) (boxed) is 0';
is nqp::objprimspec(Str), 0, 'objprimspec(Str) (boxed) is 0';

class HasNative { has int $.x; has Str $.y; }
for HasNative.^attributes -> $attr {
    if $attr.name eq '$!x' {
        is nqp::objprimspec($attr.type), 1, 'objprimspec of a native int attribute type';
    }
    if $attr.name eq '$!y' {
        is nqp::objprimspec($attr.type), 0, 'objprimspec of a boxed Str attribute type';
    }
}

# --- nqp::coerce_is / nqp::coerce_si ----------------------------------------
{
    my int $i = 42;
    is nqp::coerce_is($i), '42', 'coerce_is: native int to str';
}
is nqp::coerce_si("42"), 42, 'coerce_si: leading digits parse';
is nqp::coerce_si("  +5abc"), 5, 'coerce_si: skips leading space and sign, stops at non-digit';
is nqp::coerce_si("abc"), 0, 'coerce_si: no leading digits is 0';
is nqp::coerce_si("-7"), -7, 'coerce_si: negative';

# --- nqp::unbox_s ------------------------------------------------------------
is nqp::unbox_s("hello"), 'hello', 'unbox_s: native str from a boxed Str';
