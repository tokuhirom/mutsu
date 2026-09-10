use v6;
use Test;

# Two independent defects in how multi dispatch ranks a NATIVE candidate.
#
# 1. A literal had no source variable, so `unwrap_varref_for_dispatch` found no
#    `var_type` and the boxed candidate ranked at distance 0:
#
#        multi sub d(int $x) { "native" }
#        multi sub d(Int $x) { "boxed" }
#        d(5);     # raku: native   mutsu: boxed
#
#    The discriminator is PROVENANCE, not the value: raku answers `boxed` for
#    `d("7".Int)`, an in-range boxed Int produced at runtime, and for
#    `d(2**70)`, which does not fit the native width.
#
# 2. Even WITH a source variable the comparison was on the spelling, so only an
#    identically-named native constraint beat `Int`. raku ranks every native
#    type in the same FAMILY equally: `my int $n` picks `int8`, `int32` and
#    `int64` over `Int` alike. Signedness is a family boundary (a `my int $n`
#    against `uint`/`Int` answers `Int`), and so is int-vs-num.
#
# Every expectation below was measured against rakudo 2026.07.

plan 23;

# --- 1. literal provenance ---------------------------------------------
{
    multi sub d(int $x) { "native" }
    multi sub d(Int $x) { "boxed" }
    is d(5), 'native', 'an Int literal picks the native candidate';
    is d(-3), 'native', 'a negated Int literal too (it parses as Unary/Literal)';
    is d("7".Int), 'boxed', 'a runtime-produced in-range Int still picks the boxed one';
    is d(2**70), 'boxed', 'and so does one too wide for the native type';
    my int $n = 5;
    is d($n), 'native', 'a native-typed variable still picks the native one';
    my Int $b = 5;
    is d($b), 'boxed', 'and an Int-typed variable still picks the boxed one';
}

# Declaration order must not matter.
{
    multi sub e(Int $x) { "boxed" }
    multi sub e(int $x) { "native" }
    is e(5), 'native', 'the same holds with the candidates declared in the other order';
}

# The other two native families.
{
    multi sub h(num $x) { "nativenum" }
    multi sub h(Num $x) { "boxednum" }
    is h(1e0), 'nativenum', 'a Num literal picks the native num candidate';
}
{
    multi sub i(str $x) { "nativestr" }
    multi sub i(Str $x) { "boxedstr" }
    is i("a"), 'nativestr', 'a Str literal picks the native str candidate';
}

# --- 2. the native FAMILY rule -----------------------------------------
{
    multi sub j(int64 $x) { "i64" }
    multi sub j(Int $x)   { "boxed" }
    is j(5), 'i64', 'an Int literal reaches a differently-spelled signed candidate';
    my int $n = 5;
    is j($n), 'i64', 'and so does a `my int` variable';
}
{
    multi sub a1(int8 $x) { "i8" }
    multi sub a1(Int $x)  { "boxed" }
    my int $v = 5;
    is a1($v), 'i8', 'a `my int` variable reaches a NARROWER signed candidate';
    is a1(5), 'i8', 'and so does a literal';
}
{
    multi sub a2(int32 $x) { "i32" }
    multi sub a2(Int $x)   { "boxed" }
    my int $v = 5;
    is a2($v), 'i32', '... at every width in the family';
}
{
    multi sub b1(num32 $x) { "n32" }
    multi sub b1(Num $x)   { "boxed" }
    my num $v = 1e0;
    is b1($v), 'n32', 'the num family behaves the same way';
    is b1(1e0), 'n32', 'for a literal too';
}
{
    multi sub c1(uint8 $x) { "u8" }
    multi sub c1(Int $x)   { "boxed" }
    my uint $v = 5;
    is c1($v), 'u8', 'and the unsigned family';
}

# --- the family BOUNDARIES, which must not move ------------------------
# Signedness is a boundary: an integer literal's family is SIGNED, so `uint`
# and `byte` candidates lose to `Int`.
{
    multi sub m1(uint $x) { "uint" }
    multi sub m1(Int $x)  { "boxed" }
    is m1(5), 'boxed', 'an Int literal does NOT reach an unsigned candidate';
    my int $v = 5;
    is m1($v), 'boxed', 'nor does a `my int` variable';
}
{
    multi sub m2(byte $x) { "byte" }
    multi sub m2(Int $x)  { "boxed" }
    is m2(5), 'boxed', '... byte included';
}
{
    multi sub m3(int $x) { "int" }
    multi sub m3(Int $x) { "boxed" }
    my uint $v = 5;
    is m3($v), 'boxed', 'and a `my uint` variable does not reach a signed candidate';
}
# int-vs-num is a boundary too.
{
    multi sub m4(num $x) { "num" }
    multi sub m4(Int $x) { "boxed" }
    my int $v = 5;
    is m4($v), 'boxed', 'a `my int` variable does not reach a num candidate';
}

# A native candidate still beats a plain `Any`, as it always did.
{
    multi sub k1(int $x) { "native" }
    multi sub k1(Any $x) { "any" }
    is k1(5), 'native', 'a native candidate still beats Any for a literal';
}
