use Test;

plan 5;

# A runtime `does`/`but` mixin's own public attribute accessor (`has $.sum`)
# must win over a same-named Cool/List builtin method (`.sum`) on the
# underlying native value. `mixin_role_has_method` (the bypass-native-fastpath
# gate) previously only checked the role's declared METHODS, missing public
# ATTRIBUTES (whose accessor is auto-generated, not a `method` entry) -- so
# `$q.sum` fell through to the builtin `List.sum`/`Cool.sum` on the mixed-in
# Int instead of reading the role's own attribute.
# See news/2026-08/mixin-attribute-named-sum-shadowed-by-builtin.md.

{
    role R[$a, $b] { has $.sum; submethod TWEAK { $!sum = $a + $b } }
    my $q = 1;
    $q does R[10, 20];
    is $q.sum, 30, 'mixin attribute named sum wins over builtin Cool.sum';
}

{
    # A non-parametric role variant of the same shape.
    role R3 { has $.max; submethod TWEAK { $!max = 99 } }
    my $q = 1;
    $q does R3;
    is $q.max, 99, 'mixin attribute named max wins over builtin List.max';
}

{
    # Regression guard: class-based composition (not a runtime mixin) already
    # worked before this fix and must keep working.
    role R2 { has $.sum; }
    class D does R2 { submethod TWEAK { $!sum = 42 } }
    is D.new.sum, 42, 'class-composed role attribute named sum still works';
}

{
    # Regression guard: a genuinely builtin method call on a plain (non-mixin)
    # value is unaffected.
    is (1, 2, 3).sum, 6, 'plain List.sum is unaffected';
}

{
    # A mixin role with NO attribute/method of a builtin's name still falls
    # through to the builtin correctly (the bypass check must not become
    # overly eager).
    role R4 { has $.label; }
    my @a = (1, 2, 3);
    @a does R4;
    is @a.sum, 6, 'unrelated mixin role does not block an unrelated builtin method';
}
