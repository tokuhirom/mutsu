use Test;

plan 4;

# `apply_role_mixin`'s attribute-default-construction loop
# (src/runtime/types/roles.rs) did not run the default expression through
# `coerce_attr_value_by_sigil`, unlike every other attribute-default site
# (instance construction, run_role_submethod). For a `%`-sigiled attribute,
# a bare-Pair-list default like `(x => 1)` must coerce to a Hash the same
# way it does for an ordinary class instance. Checked against Rakudo v2026.06.

role WithHashDefault { has %.h = (x => 1); }
my $u = 1;
my $u2 = $u but WithHashDefault;
is-deeply $u2.h, %(x => 1), 'a role hash-attribute default coerces to Hash on a but-mixed plain value';
isa-ok $u2.h, Hash, 'the coerced default is a Hash, not a bare Pair';

# @-sigiled defaults were already correct (a List literal needs no coercion
# to be an acceptable Positional) -- pin the non-regression alongside the fix.
role WithArrayDefault { has @.a = (1, 2); }
my $v = 1;
my $v2 = $v but WithArrayDefault;
is-deeply $v2.a, [1, 2], 'a role array-attribute default stays an Array on a but-mixed plain value';
isa-ok $v2.a, Array, 'the array default is an Array';
