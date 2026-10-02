use Test;

# `++`/`--` on a package-qualified variable (`$GLOBAL::n++`) that the write
# itself creates must outlive the frame it ran in. The statement is the same
# whether it sits in a role body composed from inside a routine, in the body of
# a class declared in a routine, or in an EVAL: each runs in a nested frame
# whose env is restored on exit, and the read-modify-write store used to write
# only that env (`=` and `+=` persisted theirs). Expected values are rakudo's.

plan 13;

# --- the reported shape ------------------------------------------------------
{
    role CountR { $GLOBAL::n1++ }
    sub f { my class CountC does CountR { } }
    f();
    is $GLOBAL::n1, 1, 'a role body composed from a routine keeps its `$GLOBAL::n++`';
}
{
    role Same { $GLOBAL::n2++ }
    class Top does Same { }
    is $GLOBAL::n2, 1, 'composed at unit level it already did';
}

# --- every operator of the family --------------------------------------------
{
    role Post { $GLOBAL::p1++ }
    role Pre { ++$GLOBAL::p2 }
    role Dec { $GLOBAL::p3-- }
    role PreDec { --$GLOBAL::p4 }
    sub g { my class A does Post { }; my class B does Pre { }; my class C does Dec { }; my class D does PreDec { } }
    g();
    is-deeply ($GLOBAL::p1, $GLOBAL::p2, $GLOBAL::p3, $GLOBAL::p4), (1, 1, -1, -1),
        'postfix and prefix ++ and --';
}
{
    role Plus { $GLOBAL::q1 += 2 }
    role Assign { $GLOBAL::q2 = 7 }
    sub h { my class A does Plus { }; my class B does Assign { } }
    h();
    is-deeply ($GLOBAL::q1, $GLOBAL::q2), (2, 7), '`+=` and `=` still persist';
}

# --- a class body in a routine, and an EVAL ----------------------------------
{
    sub body { my class K { $GLOBAL::k1++ } }
    body();
    is $GLOBAL::k1, 1, 'a class body in a routine';
    body();
    is $GLOBAL::k1, 2, '... counts on from the earlier call, not from zero';
}
{
    sub ev { EVAL q[$GLOBAL::e1++] }
    ev();
    is $GLOBAL::e1, 1, 'an EVAL in a routine';
    ev();
    is $GLOBAL::e1, 2, '... counts on from the earlier call';
}
{
    sub assign-then-bump { EVAL q[$GLOBAL::e2 = 7; $GLOBAL::e2++] }
    assign-then-bump();
    is $GLOBAL::e2, 8, 'an `=` and then a `++` in one EVAL: the `++` lands';
}
{
    sub stash { EVAL q[$GLOBAL::e3++] }
    stash();
    is GLOBAL::<$e3>, 1, 'the value is the one the GLOBAL stash holds';
}

# --- other package names -----------------------------------------------------
{
    sub qualified { my class Q { $Foo::x++ } }
    qualified();
    is $Foo::x, 1, 'a `Pkg::` qualified name in a class body in a routine';
    qualified();
    is $Foo::x, 2, '... and on a second call';
}
{
    sub plain { $Bar::y++ }
    plain();
    plain();
    is $Bar::y, 2, 'the plain routine form keeps counting';
}
