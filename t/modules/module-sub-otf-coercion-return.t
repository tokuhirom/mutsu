use lib $*PROGRAM.parent.child("lib").Str;
use Test;
use CoercionReturnOtf;

# Pins that module subs with coercion return types (`--> Str()`,
# `--> Foo:D()`, coercive subsets) behave raku-identically when
# OTF-compiled (def_is_otf_compilable_module_single) — the coercion
# exclusion was lifted 2026-07-12 (PLAN §3 fallback removal).

plan 15;

is cr-str(42), "42", "Str() coerces an Int";
isa-ok cr-str(42), Str, "Str() result is a Str";
is cr-int("17") + 1, 18, "Int() coerces a Str";
isa-ok cr-int("17"), Int, "Int() result is an Int";

# interleaved calls to several coexisting coercion subs
is (cr-str(1), cr-int("2"), cr-str(3)).join("|"), "1|2|3",
    "several coercion subs coexist without cross-pollution";

is cr-str-numeric(42.12), "42.12", "Str(Numeric:D) coerces a Rat";
is cr-str-numeric("keep"), "keep", "matching value bypassed as-is";
is-deeply cr-str-numeric(Nil), Nil, "Nil is returned as-is";
throws-like { cr-str-numeric([1,2]) }, X::TypeCheck::Return,
    "non-Numeric throws X::TypeCheck::Return";
throws-like { cr-str-numeric(Int) }, X::TypeCheck::Return,
    "type object throws X::TypeCheck::Return";

isa-ok cr-custom(42), Wrapped, "custom COERCE dispatches";
is cr-custom(42).val, 42, "custom COERCE receives the value";

is cr-subset("0.2"), 0.2, "coercive subset coerces";
throws-like { cr-subset("1.3") }, X::Coerce::Impossible,
    "coercive subset rejects out-of-range via X::Coerce::Impossible";
throws-like { cr-subset(1.3) }, X::Coerce::Impossible,
    "value failing subset constraint throws";
