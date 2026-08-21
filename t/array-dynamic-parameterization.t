use v6;
use Test;

# `array[T]` (the lowercase native shaped/typed array declarator used as a
# term) must accept a DYNAMIC/computed type-parameter expression, not just a
# compile-time-literal type name. Only the literal spelling (`array[int8]`)
# was special-cased by the compiler (which synthesizes the parameterized
# type name directly); a runtime index expression
# (`array[$cond ?? int8 !! uint8]`) fell through to the VM's generic
# Package-indexing path, which threw X::NotParametric ("array cannot be
# parameterized") because `is_non_parametric_type`'s allowlist had `"Array"`
# (capitalized boxed Array) but not lowercase `"array"`.
#
# CBOR::Simple's RFC 8746 typed-array decoder relies on exactly this shape
# to pick a signed/unsigned element type at runtime:
# `array[$is-signed ?? int8 !! uint8].new`.

plan 5;

my $is-signed = True;
my $signed-array := array[$is-signed ?? int8 !! uint8].new;
is $signed-array.of, int8, 'dynamic array[T] parameterization picks the True branch type';

my $is-signed2 = False;
my $unsigned-array := array[$is-signed2 ?? int8 !! uint8].new;
is $unsigned-array.of, uint8, 'dynamic array[T] parameterization picks the False branch type';

# The literal (compile-time-known) spelling must still work identically.
my $literal-array := array[num32].new;
is $literal-array.of, num32, 'literal array[T] parameterization still works';

# The dynamically-parameterized array is a genuine native typed array: it
# type-checks and stores elements correctly.
$signed-array.push(5);
$signed-array.push(-3);
is-deeply $signed-array.List, (5, -3), 'dynamically-parameterized array stores elements';

$unsigned-array.push(200);
is $unsigned-array[0], 200, 'dynamically-parameterized unsigned array stores elements';

# NOTE: native-width wrapping on `.push` (e.g. push(-1) -> 255 for uint8) is
# NOT yet covered for a dynamically-parameterized array — that wrap logic is
# keyed off the lexical variable's compile-time-registered type constraint,
# which a `:=`-bound `array[$cond ?? T1 !! T2].new` value never gets. Tracked
# separately in todo/tickets/dynamic-array-parameterization-push-wrap.md —
# out of scope here since CBOR::Simple's own decoder never pushes an
# out-of-range value onto one of these (its `06-typed-arrays.rakutest`
# passes in full without it).
