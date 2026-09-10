use Test;

# An `our multi` candidate is only legal when an `our proto` declares the multi,
# and mutsu enforces that by looking the proto up in `proto_subs`. A package
# body's proto is registered by the body's own `RegisterProtoSub`, which has not
# run when the CHECK-time inline-package prepass installs the candidates — so a
# `module`/`package` body was rejected outright where a `class` body, and
# mainline, both worked.
#
# The prepass now publishes the body's protos before its candidates, and tells
# the in-sequence registration that the entry it finds is its own
# pre-registration (the proto twin of the existing
# `__mutsu_inline_package_sub_preregistered` protocol) rather than a
# redeclaration.

plan 10;

module M2 { our proto sub f4($) {*}; our multi sub f4(Int $x) { "int" } }
is M2::f4(1), 'int', 'a module body`s `our proto` + `our multi` resolves';

package P1 { our proto sub f5($) {*}; our multi sub f5(Int $x) { "int" } }
is P1::f5(1), 'int', 'and so does a package body`s';

module M6 {
    our proto sub f9($) {*};
    our multi sub f9(Int $x) { "int" }
    our multi sub f9(Str $x) { "str" }
}
is M6::f9(1), 'int', 'several candidates dispatch by type (Int)';
is M6::f9("x"), 'str', '... and (Str)';

# --- shapes that already worked, kept as controls ---------------------------
{
    proto sub f1($) {*};
    our multi sub f1(Int $x) { "int" }
    is f1(1), 'int', 'control: mainline';
}
class C1 { our proto sub f2($) {*}; our multi sub f2(Int $x) { "int" } }
is C1::f2(1), 'int', 'control: a class body';

module M1 { our proto sub f3($) {*}; multi sub f3(Int $x) { "int" } }
is M1::f3(1), 'int', 'control: a module body with `my`-scoped candidates';

# --- ... and the two things that must still be refused ----------------------
{
    my $err;
    { EVAL 'module ME1 { our multi sub g1(Int $x) { "x" } }'; CATCH { default { $err = .message } } }
    like $err, /'individual multi candidates'/, 'an `our multi` with no proto at all is still refused';
}
{
    my $err;
    { EVAL 'module ME2 { our proto sub g2($) {*}; our proto sub g2($) {*} }';
      CATCH { default { $err = .message } } }
    like $err, /'Redeclaration'/, 'a duplicate `our proto` in one body is still refused';
}
{
    # The prepass marker is consumed by the body's own registration, so a
    # SECOND module declaring the same package name still gets a fresh check.
    my $err;
    { EVAL 'module ME3 { our proto sub g3($) {*}; our multi sub g3(Int $x) { "x" } };
            module ME3 { our proto sub g3($) {*} }';
      CATCH { default { $err = .message } } }
    like $err, /'Redeclaration'/, 'and so does a re-opened package body';
}
