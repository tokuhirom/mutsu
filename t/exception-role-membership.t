use v6;
use Test;

# ADR-0029 Slice 1: X:: exception ancestry is role membership, registered
# through the existing composed-role path (registry.class_composed_roles /
# role_parents), not folded into a class's single-inheritance `parents`/`mro`.
# X::Comp::AdHoc is the first (and, in Slice 1, only) migrated class: in real
# rakudo its superclass is X::AdHoc and it *does* X::Comp -- the opposite of
# what the old hand-spliced workaround modelled.

is X::Comp::AdHoc.^mro.map(*.^name).join(' '), 'X::Comp::AdHoc X::AdHoc Exception Any Mu',
    '.^mro is the real superclass chain, no X::Comp in it';

ok (X::Comp::AdHoc.^roles.map(*.^name).grep('X::Comp')), '.^roles contains X::Comp';

my $e = X::Comp::AdHoc.new;
ok $e ~~ X::AdHoc, 'instance ~~ X::AdHoc (the real superclass)';
ok $e ~~ X::Comp, 'instance ~~ X::Comp (the composed role)';

ok X::Comp::AdHoc ~~ X::AdHoc, 'bare type object ~~ X::AdHoc';
ok X::Comp::AdHoc ~~ X::Comp, 'bare type object ~~ X::Comp (resolve_role_key gate)';

ok X::Comp::AdHoc.^does(X::Comp), '.^does(X::Comp) agrees with ~~';
nok X::Comp::AdHoc.^does(X::Numeric), '.^does is not vacuously true';

# The 14 role-shaped `X::` marker roles must never leak into a class's MRO --
# that was exactly the bug the old splice risked generalising.
nok X::Comp::AdHoc.^mro.map(*.^name).grep('X::Comp'), 'X::Comp is not an MRO entry (does-composed, not inherited)';

done-testing;
