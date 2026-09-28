use v6;
use Test;

# `but role` on a List kept the List's own `.^name` before composing the
# mixin, but mutsu's `apply_but_mixin`/`compose_role_on_value` path collapsed
# any non-itemized `Array` view (both `ArrayKind::List` and `ArrayKind::Array`)
# to "Array" when building the mixin's base name, so `List+{R}` misreported
# as `Array+{R}` (`value/types.rs`'s `what_type_name` is also used for other
# `Mixin` bases like `Package`, so the fix only special-cases the Array/List
# split rather than swapping in the stricter `value_type_name` wholesale).
#
# Separately, `join`'s slurpy pre-render step (`join_prerender_user_stringifier`
# in `runtime/builtins_collection_listops.rs`) stringified a container-inner
# mixin (`@a but role { ... }`) as ONE unit via its role's `.Str`, before the
# slurpy had a chance to flatten it into its elements — `flat_val` itself
# already flattens a container-inner mixin through its inner value (the
# mixin's own `Str` override does not apply to the elements a flattening
# slurpy exposes instead of the whole value), so the pre-render step needed
# the same rule.
#
# Verified against rakudo 2026.07.

plan 7;

role R {}
is (<a b> but R).^name, 'List+{R}', 'but role on a List literal keeps List, not Array';

my $l = (1, 2);
is ($l but R).^name, 'List+{R}', 'but role on a scalar-held List keeps List';

my @arr = 1, 2;
is (@arr but R).^name, 'Array+{R}', 'but role on a real Array still reports Array';

my @o = 3, 2, 1;
is join('>', @o but role { method Str { 'X' } }), '3>2>1',
    'join flattens a but-mixed Array into its elements, ignoring the mixin Str';

is (@o but role { method Str { 'X' } }).Str, 'X',
    'the mixin Str still applies when the whole value is stringified directly';

is join(',', 1 but role { method Str { 'S!' } }), 'S!',
    'join still honours a scalar mixin Str (its inner is not a container)';

is join(',', @o but role { method Str { 'X' } }, @o), '3,2,1,3,2,1',
    'a but-mixed Array followed by a plain Array still flattens both';

done-testing;
