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

# ADR-0029 Slice 3: the ~44 existing false-superclass rows (X::Syntax::Confused
# inheriting X::Syntax was the ADR's headline example) are corrected the same
# way, and ~257 previously-unregistered classes land using the same mechanism.

is X::Syntax::Confused.^mro.map(*.^name).join(' '), 'X::Syntax::Confused Exception Any Mu',
    'X::Syntax::Confused no longer false-inherits X::Syntax';
is X::Syntax::Confused.^roles.map(*.^name).join(' '), 'X::Syntax X::Comp',
    '.^roles is transitive: does X::Syntax, which itself does X::Comp';
ok X::Syntax::Confused ~~ X::Syntax, 'does the direct role';
ok X::Syntax::Confused ~~ X::Comp, 'does the transitively-composed role';

# A previously-unregistered class (Slice 3's ~257 additions) constructs and
# reports the real rakudo ancestry.
is X::Coerce::Impossible.^mro.map(*.^name).join(' '), 'X::Coerce::Impossible X::Coerce Exception Any Mu',
    'newly-registered class has its real intermediate ancestor';

# throws-like's X::Comp::Group fallback (src/runtime/test_functions/throws_like.rs)
# used to check only .mro for an X::Comp ancestor; corrected classes that DO
# (rather than inherit) X::Comp need the composed-role-aware check too.
throws-like 'given 42 { when SomeUndeclaredType { 1 }; default { 0 } }',
    X::Comp::Group, 'X::Undeclared::Symbols (does X::Comp, does not inherit it) still matches X::Comp::Group in throws-like';

# ADR-0029 residue R1: role-to-role composition among the 16 marker roles
# themselves, cross-checked against real raku (2026-08-19) -- exactly three
# edges exist (X::Syntax does X::Comp, X::IO does X::OS, X::Role::Attribute
# does X::RoleApplier); the other thirteen compose nothing. Slice 3 grew the
# marker-role list from 14 to 16 without re-running this measurement, which is
# how the third edge was missed.
is X::Syntax.^roles.map(*.^name).join(' '), 'X::Comp', 'X::Syntax does X::Comp (role-to-role edge)';
is X::IO.^roles.map(*.^name).join(' '), 'X::OS', 'X::IO does X::OS (role-to-role edge)';
is X::Role::Attribute.^roles.map(*.^name).join(' '), 'X::RoleApplier',
    'X::Role::Attribute does X::RoleApplier (role-to-role edge)';
is X::Comp.^roles.map(*.^name).join(' '), '', 'X::Comp composes no further roles (leaf marker)';

# The observable consequence: classes that compose X::Role::Attribute
# transitively also compose X::RoleApplier, so `~~ X::RoleApplier` must
# answer True, not False.
ok X::Role::Attribute::Conflicts ~~ X::RoleApplier,
    'X::Role::Attribute::Conflicts ~~ X::RoleApplier (transitive through X::Role::Attribute)';
ok X::Role::Attribute::Exists ~~ X::RoleApplier,
    'X::Role::Attribute::Exists ~~ X::RoleApplier (transitive through X::Role::Attribute)';
ok X::Role::Attribute::Conflicts.^does(X::RoleApplier), '.^does(X::RoleApplier) agrees with ~~';

# ADR-0029 residue R2: `X::TooLateForREPR` is rakudo's one "role-as-superclass
# pun" -- `X::Comp` is simultaneously a real MRO entry AND a composed role for
# this single class (the sole documented exception to "a marker role name
# never appears in a class's `.^mro`"; cross-checked against real raku
# 2026-08-19). It used to be the last unconstructible `X::` class
# (`X::Method::NotFound ... new on X::TooLateForREPR`).
is X::TooLateForREPR.new.^name, 'X::TooLateForREPR', 'X::TooLateForREPR.new succeeds';
is X::TooLateForREPR.^mro.map(*.^name).join(' '), 'X::TooLateForREPR X::Comp Exception Any Mu',
    'X::TooLateForREPR.^mro includes X::Comp as a real ancestor (the pun)';
is X::TooLateForREPR.^roles.map(*.^name).join(' '), 'X::Comp',
    'X::TooLateForREPR.^roles also reports X::Comp (the same pun, as a role)';
ok X::TooLateForREPR ~~ X::Comp, 'bare type object ~~ X::Comp';
ok X::TooLateForREPR.new ~~ X::Comp, 'instance ~~ X::Comp';

done-testing;
