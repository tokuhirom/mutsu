use v6;
use Test;

# A role-mixed value's definedness (`.defined`, and `:U`/`:D` signature
# smileys) must follow what it wraps: `Any but role {...}` is a type object
# (undefined, `:U`), while `Any.new but role {...}` is a concrete instance
# (defined, `:D`) -- the mixin wrapper itself carries no definedness of its
# own.
#
# Regression: `value_is_defined` had no `Mixin` arm, so EVERY mixin (whether
# wrapping an undefined type-object `Package` or a defined `Instance`) fell
# through to its `_ => true` default and was reported as defined. This broke
# `:U`-constrained parameter binding for a role-mixed type object -- e.g. the
# real Test.rakumod's `is(Mu $got, Mu:U $expected, ...)` multi candidate,
# selected for an undefined `$expected`, could never bind a mixin type
# object, so the WRONG `is` multi ran and mis-rendered its diagnostic. See
# roast/6.c/S14-roles/mixin-6c.t tests 48-49, which only fail under
# MUTSU_REAL_TEST=1 (the vendored real Test.rakumod), not the native
# provider, since the native provider's `is` doesn't dispatch through this
# multi/parameter-binding path at all.

plan 8;

my $type_obj = Any but role Meows { method Bool { True } };
is $type_obj.defined, False, 'a mixin on a type object is still undefined';

sub wants-undefined(Mu:U $x) { $x.^name }
is wants-undefined($type_obj), 'Any+{Meows}',
    'a :U-constrained parameter accepts a mixin type object';

my $instance = Any.new but role Meows2 { method Bool { True } };
is $instance.defined, True, 'a mixin on an instance is still defined';

sub wants-defined(Mu:D $x) { $x.^name }
is wants-defined($instance), 'Any+{Meows2}',
    'a :D-constrained parameter accepts a mixin instance';

# `//` and `orelse` must also see the mixin type object as "nothing here".
my $fallback = $type_obj // 'fallback';
is $fallback, 'fallback', '// falls through for an undefined mixin type object';

my $kept = $instance // 'fallback';
is $kept.^name, 'Any+{Meows2}', '// keeps a defined mixin instance';

# A custom `method defined` override on the mixed-in role still wins over
# the structural check (pre-existing behavior, pinned so it can't regress
# alongside the structural fix above).
my $forced-defined = Any but role Forced { method defined { True } };
is $forced-defined.defined, True,
    'a role-supplied .defined override on a type object mixin still wins';

my $forced-undefined = Any.new but role ForcedU { method defined { False } };
is $forced-undefined.defined, False,
    'a role-supplied .defined override on an instance mixin still wins';

done-testing;
