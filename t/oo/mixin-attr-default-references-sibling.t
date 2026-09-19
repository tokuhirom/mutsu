use Test;

# A role attribute's default expression may reference an earlier attribute of
# the SAME role through its accessor (AttrX::Lazy's `LazyAttribute` role does
# exactly this: `has $.builder is rw = "build_" ~ self.base-name`). `but`/
# `does` on a non-Instance value (`compose_role_on_value`) used to get this
# wrong (#8806): it bound `self` to a snapshot of the mixin map taken BEFORE
# the attribute loop started, so a later attribute's default saw none of the
# earlier ones. (The equivalent bug for `does` on a real object, the
# in-place rebless path, is pinned separately in
# t/oo/role/dependent-role-attribute-default.t.)

plan 2;

role Greeting {
    has $.first = "hello";
    has $.second = self.first ~ " world";
}

my $wrapped = 0 but Greeting;
is $wrapped.second, "hello world",
    'but-mixin: later attribute default sees an earlier sibling via self';

my $bound = 0;
$bound does Greeting;
is $bound.second, "hello world",
    'does-mixin on a scalar: later attribute default sees an earlier sibling';
