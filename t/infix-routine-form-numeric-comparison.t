use Test;

# The numeric-comparison family (`==`, `!=`, `<`, `>`, `<=`, `>=`, `<=>`) must
# behave identically whether reached as the operator (`$a == $b`) or as the
# ROUTINE `&infix:<==>($a, $b)` -- exactly how the real vendored upstream
# `Test.rakumod`'s `cmp-ok` reaches an operator, via
# `&CALLER::LEXICAL::("infix:<$op>")`. Before this fix, the routine form fell
# through to a pure static fold with its own, INCOMPLETE numeric-coercion
# bridge, so it disagreed with the operator on: Inf-valued Rat/FatRat, a
# SetHash compared against an Array, and an instance of a user subclass of
# Int. Same story for the reduction/meta forms (`[==]`, `Z==`, `>>==<<`),
# which shared the same static fold via `eval_reduction_operator_values`.

plan 27;

# --- == -----------------------------------------------------------------

{
    my $inf-rat = Inf.Rat;
    ok $inf-rat == Inf, 'operator: Inf.Rat == Inf';
    ok &infix:<==>($inf-rat, Inf), 'routine: &infix:<==>(Inf.Rat, Inf)';
}

{
    my $neg-inf-fatrat = (-Inf).FatRat;
    ok $neg-inf-fatrat == -Inf, 'operator: (-Inf).FatRat == -Inf';
    ok &infix:<==>($neg-inf-fatrat, -Inf), 'routine: &infix:<==>((-Inf).FatRat, -Inf)';
}

{
    my @a = 1, 2, 3;
    my $s = SetHash.new(1, 2, 3);
    ok @a == $s, 'operator: Array == SetHash (same elements)';
    ok &infix:<==>(@a, $s), 'routine: &infix:<==>(Array, SetHash)';
}

{
    my class RoutineFormSubclassOfInt is Int { }
    my $obj = RoutineFormSubclassOfInt.new(42);
    ok $obj == 42, 'operator: subclass-of-Int instance == 42';
    ok &infix:<==>($obj, 42), 'routine: &infix:<==>(subclass-of-Int instance, 42)';
}

# --- != -------------------------------------------------------------------

{
    my $inf-rat = Inf.Rat;
    nok $inf-rat != Inf, 'operator: Inf.Rat != Inf is False';
    nok &infix:<!=>($inf-rat, Inf), 'routine: &infix:<!=>(Inf.Rat, Inf) is False';
    ok &infix:<!=>($inf-rat, -Inf), 'routine: &infix:<!=>(Inf.Rat, -Inf) is True';
}

# --- < > <= >= --------------------------------------------------------------

{
    my $inf-rat = Inf.Rat;
    ok 1.Rat < $inf-rat, 'operator: 1.Rat < Inf.Rat';
    ok &infix:«<»(1.Rat, $inf-rat), 'routine: &infix:«<»(1.Rat, Inf.Rat)';

    ok $inf-rat > 1.Rat, 'operator: Inf.Rat > 1.Rat';
    ok &infix:«>»($inf-rat, 1.Rat), 'routine: &infix:«>»(Inf.Rat, 1.Rat)';

    ok $inf-rat <= Inf, 'operator: Inf.Rat <= Inf';
    ok &infix:«<=»($inf-rat, Inf), 'routine: &infix:«<=»(Inf.Rat, Inf)';

    ok $inf-rat >= Inf, 'operator: Inf.Rat >= Inf';
    ok &infix:«>=»($inf-rat, Inf), 'routine: &infix:«>=»(Inf.Rat, Inf)';
}

# --- <=> --------------------------------------------------------------------

{
    my $inf-rat = Inf.Rat;
    is $inf-rat <=> Inf, Same, 'operator: Inf.Rat <=> Inf is Same';
    is &infix:«<=>»($inf-rat, Inf), Same, 'routine: &infix:«<=>»(Inf.Rat, Inf) is Same';
}

# --- reduction / metaop forms share the same fix ---------------------------

{
    my $inf-rat = Inf.Rat;
    my $result = [==] ($inf-rat, Inf, Inf);
    ok $result, '[==] over Inf.Rat, Inf, Inf';
}

{
    my @a = Inf.Rat, 1;
    my @b = Inf, 1;
    is-deeply (@a Z== @b), (True, True), 'Z== zips == elementwise';
}

{
    my @a = Inf.Rat, 1;
    my @b = Inf, 1;
    is-deeply (@a >>==<< @b), [True, True], '>>==<< hypers == elementwise';
}

# --- a user-defined infix:<==> multi still wins over this fix's redirect ---
#
# Routing `[==]`/`Z==`/`>>==<<` and `&infix:<==>(...)` through the real
# operator body must NOT bypass a `multi sub infix:<==>` the user declared
# for their own class -- that candidate has to keep winning over the
# built-in numeric-comparison redirect, exactly as it did before this fix.

{
    my class RoutineFormUserOverrideTarget {
        has $.v;
    }
    multi sub infix:<==>(RoutineFormUserOverrideTarget $a, RoutineFormUserOverrideTarget $b) {
        $a.v == ($b.v + 1000)
    }
    my $a = RoutineFormUserOverrideTarget.new(:v(1));
    my $b = RoutineFormUserOverrideTarget.new(:v(-999));
    ok $a == $b, 'operator: user-defined infix:<==> still wins';
    ok &infix:<==>($a, $b), 'routine: user-defined infix:<==> still wins';
    my $reduced = [==] ($a, $b);
    ok $reduced, '[==]: user-defined infix:<==> still wins';
}
