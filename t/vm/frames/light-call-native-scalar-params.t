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

plan 22;

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
