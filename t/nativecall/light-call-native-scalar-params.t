use Test;

# #8686 Phase 0: `int`/`str`/`num` (the plain, full-width lowercase native
# scalar spellings) are now `FastParamType` tags of their own
# (`NativeInt`/`NativeStr`/`NativeNum`), so a routine using them for its
# scalar params now reaches the light/positional-light call fast paths
# instead of always falling back to the general binder. This pins that the
# fast paths reproduce the general binder's own behavior for these three
# spellings: `Bool` unboxes to `Int` for `int` (but stays rejected for `str`/
# `num`), a bare type object is rejected (unlike a boxed `Int $x`, which
# accepts one), an out-of-range `BigInt` still raises the general binder's own
# overflow message, and the per-parameter `~~ int`-style declared-type
# metadata still works from inside the body.

plan 27;

sub takes-int(int $x) { $x }
is takes-int(5), 5, 'native int parameter accepts a plain Int';
is takes-int(True), 1, 'native int parameter unboxes True to 1';
is takes-int(True).^name, 'Int', 'the unboxed value is a boxed Int';
is takes-int(False), 0, 'native int parameter unboxes False to 0';

sub takes-str(str $x) { $x }
is takes-str("hi"), "hi", 'native str parameter accepts a plain Str';
my $five = 5;
dies-ok { takes-str($five) }, 'native str parameter rejects an Int (no Bool-style coercion)';

sub takes-num(num $x) { $x }
is takes-num(1e0), 1e0, 'native num parameter accepts a plain Num';
dies-ok { takes-num($five) }, 'native num parameter rejects an Int';

# A bare type object cannot be unboxed to a native type at all -- unlike the
# boxed `Int $x` case (`light-call-bare-type-accepts-type-object.t`), which
# legitimately accepts it.
dies-ok { takes-int(Int) }, 'native int parameter rejects the bare Int type object';
dies-ok { takes-str(Str) }, 'native str parameter rejects the bare Str type object';

# Values the native type cannot represent at all still raise an error rather
# than binding something silently wrong. Routed through a variable (rather
# than a literal argument) so raku's own static candidate check does not
# reject the call at COMPILE time -- the point here is mutsu's runtime
# behavior, matching what raku itself does at runtime for the same value.
my $abc = "abc";
dies-ok { takes-int($abc) }, 'native int parameter rejects a Str';
dies-ok { takes-int(3.5) }, 'native int parameter rejects a non-integer Rat';

# An out-of-range BigInt still raises the general binder's own overflow
# message (`wrap_native_int_for_binding`), not a generic type mismatch.
throws-like { takes-int(99999999999999999999999999999999) },
    X::AdHoc, message => /'unbox' .* 'bit wide bigint'/,
    'an out-of-range BigInt argument raises the overflow error';

# The routine keeps working correctly after a failed call.
is takes-int(9), 9, 'native int parameter still binds correctly after a failed call';

# Declared-type introspection inside the body still sees the native type
# (env-scoped `__mutsu_type::` metadata, mirrored onto the light-call fast
# path -- see `t/nativecall/native-value-smartmatch.t` subtest 25 for the
# general-binder pin this mirrors).
sub smartmatches-int(int $x) { $x ~~ int }
ok smartmatches-int(5), 'a native int parameter still smartmatches ~~ int from inside the body';

# The declared-type metadata is scoped to the callee's own frame and must not
# leak onto a caller lexical of the same name.
{
    my $x = "outer";
    sub shadow-native-int(int $x) { $x }
    is shadow-native-int(3), 3, 'a native int parameter shadows a same-named outer lexical';
    is $x, "outer", 'the caller lexical is untouched after the call';
    nok $x ~~ int, 'the caller lexical does not pick up the callee native-type metadata';
}

# A mixed positional+named signature (the light, not positional-light, path)
# applies the same coercion and metadata registration to its positional
# native param.
sub mixed-native(int $x, :$y) { "$x|" ~ ($y // 0) }
is mixed-native(True, y => 3), '1|3', 'a native int positional param in a mixed signature still coerces';

# `returns int`/`returns str`/`returns num` is not enforced by the general
# binder today (a `Bool` body value passes through a `returns int` sub
# unconverted); admitting these onto the light-call return check must match
# that same non-enforcement rather than newly rejecting or coercing it. This
# is a PRE-EXISTING mutsu gap versus real Rakudo (which does coerce the
# return value) that Phase 0 does not attempt to fix -- only parameter
# binding is in scope here.
sub native-return() returns int { True }
is native-return(), True, 'a native `returns int` does not coerce or reject a Bool return value';
is native-return().^name, 'Bool', 'the Bool return value is unconverted';

# Repeated calls through the light-call resolution/name cache keep applying
# the coercion every time (not just the first, uncached call).
my @seen;
for ^3 { @seen.push(takes-int(True)) }
is-deeply @seen, [1, 1, 1], 'cached light-call binds keep coercing True on every call';

# An allomorph (`IntStr`/`NumStr`/...) must satisfy a native constraint in
# exactly the cases it satisfies the corresponding boxed one: `isa_check`
# only ever matches the BOXED MRO name (`Value::isa_or_does_check`'s
# `my_type` is always `"Int"`/`"Str"`/`"Num"`, never the lowercase native
# spelling), so checking a `Mixin` against its own native `name_sym` would
# wrongly reject every allomorph argument -- caught by
# `t/concurrency/thread-lock/thread-clone-program-table-isolation.t` and the
# two `t/modules/batteries/*-battery.t` smoke tests going through
# `MIME::Base64.encode-str`, whose `add-byte(str $x, ...)` helper is called
# with an `IntStr` literal in real-world module code.
my $int-str = <42>;
is $int-str.^name, 'IntStr', 'sanity: <42> is an IntStr allomorph';
is takes-str($int-str), '42', 'native str parameter accepts an IntStr allomorph';
is takes-int($int-str), 42, 'native int parameter accepts an IntStr allomorph';

# An enum value satisfies the base type its values carry (`our Str enum S
# «:A<a>»` -- `S::A` is a `Str`) for a NATIVE constraint too, exactly as it
# does for the corresponding boxed one -- the `fast_type_check`/
# `fast_type_check_tagged` enum arms special-cased `Int`/`Str` but not
# `NativeInt`/`NativeStr`, so an Int-valued enum constant (the idiomatic
# `enum CBORMajorType (CBOR_UInt => 0, ...)` shape the vendored CBOR::Simple
# module uses to name its wire-format tags) was wrongly rejected by a native
# `int` parameter -- caught by the release-time battery gate
# (scripts/battery-testsuite.sh) regressing CBOR::Simple's own upstream
# suite below its recorded baseline.
enum Flavor (VANILLA => 1, CHOCOLATE => 2);
our Str enum Grade «:A<a> :B<b>»;
# mutsu does not yet unbox an accepted enum value to a plain Int at the
# native-int bind site (a PRE-EXISTING gap shared with the general binder --
# `sub f(int $x is rw) {...}` forces the general path and shows the same
# `Flavor`-not-`Int` result -- so this is not a regression this PR
# introduces, and not in scope to fix here). What CBOR::Simple's own code
# actually depends on is arithmetic behaving correctly, which it does.
ok takes-int(VANILLA) == 1, 'native int parameter accepts an Int-valued enum constant (numeric equality)';
is takes-str(Grade::A), 'a', 'native str parameter accepts a Str-valued enum constant';
