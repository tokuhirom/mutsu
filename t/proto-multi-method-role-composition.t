use Test;

# A role that declares a `proto method` together with `multi method`
# candidates of the same name, composed into a class. Previously the role
# body's proto registration ran once at role declaration and again when a
# class did the role; the second run hit the already-registered package proto
# and wrongly raised `X::Redeclaration` ("Did you mean to declare a
# multi-sub?"). A `proto method` is a method-level proto (dispatched via the
# type's method table), so it must not be registered as a package proto sub.
# Regression for lizmat's `Enumify` proto+multi pattern (SBOM::CycloneDX).

plan 7;

# proto + single multi method in a role, composed into a class.
{
    role R1 {
        proto method setup(|) {*}
        multi method setup(Str:D $key) { "str:$key" }
    }
    my class C1 does R1 { }
    is C1.setup("a"), 'str:a', 'role proto + single multi method composes and dispatches';
}

# proto + multiple multi methods in a role, composed.
{
    role R2 {
        proto method setup(|) {*}
        multi method setup(Str:D $key) { "str" }
        multi method setup(@x) { "arr" }
    }
    my class C2 does R2 { }
    is C2.setup("a"), 'str', 'role proto + multiple multi methods (Str candidate)';
    is C2.setup([1, 2]), 'arr', 'role proto + multiple multi methods (@ candidate)';
}

# proto method declared directly in a class (no role) still works.
{
    class C3 {
        proto method calc(|) {*}
        multi method calc(Int:D $n) { $n * 2 }
        multi method calc(Str:D $s) { $s.uc }
    }
    is C3.calc(5), 10, 'class proto + multi method dispatches on Int';
    is C3.calc("hi"), 'HI', 'class proto + multi method dispatches on Str';
}

# Two classes composing the same role independently (the proto runs per class).
{
    role R4 {
        proto method label(|) {*}
        multi method label(Int:D $n) { "n=$n" }
    }
    my class A4 does R4 { }
    my class B4 does R4 { }
    is A4.label(1), 'n=1', 'first class composing the role dispatches';
    is B4.label(2), 'n=2', 'second class composing the same role dispatches';
}
