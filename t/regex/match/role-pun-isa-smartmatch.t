use v6;
use Test;
use nqp;

# A punned role's instance must `isa`/smartmatch its pun (the concrete class
# the role generates for its instances, `R.^pun`), even though the bare role
# itself is correctly excluded from `.isa` (roles are not nominal ancestors).
#
# Regression for a bug where `R.new.isa(R.^pun)` and `R.new ~~ R.^pun` were
# both False in mutsu (raku: True) because `R.^pun` is represented internally
# as a `Mixin`-wrapped `Package`, and the `isa`/smartmatch/`nqp::istype` code
# paths that extract a type name from their argument did not know how to
# unwrap a `Mixin` — they fell through to a generic string-fallback that
# could never match. See roast/S12-coercion/coercion-methods.t (the "Roles"
# subtest), which additionally exercises this through `Test.rakumod`'s real
# `isa-ok`, which for a non-`Str` expected type calls
# `nqp::istype($var, $type.WHAT)` rather than `.isa` directly.

plan 12;

role R1 { }

my $o = R1.new;

is $o.^name, 'R1', 'instance of a role reports the role name via .^name';
is R1.^pun.^name, 'R1', "R.^pun's own .^name is the role name too";

# The bare role itself is correctly excluded from nominal isa checks.
is $o.isa(R1), False, '.isa(R1) (the bare role) is False -- roles are not nominal ancestors';

# But the pun IS a real class, and isa/does/smartmatch against it must work.
is $o.isa(R1.^pun), True, '.isa(R1.^pun) is True';
is $o ~~ R1.^pun, True, '~~ R1.^pun is True';
is $o ~~ R1, True, '~~ R1 (the bare role) is still True (unaffected by this fix)';

# nqp::istype with a Mixin-wrapped type (a pun, or a pun's own .WHAT, which is
# also a Mixin) must unwrap the same way -- this is exactly what the real
# Test.rakumod's isa-ok does for a non-Str expected type.
is so(nqp::istype($o, R1.^pun)), True, 'nqp::istype($o, R1.^pun) is True';
is so(nqp::istype($o, R1.^pun.WHAT)), True, 'nqp::istype($o, R1.^pun.WHAT) is True';

# A role with a required attribute and custom new/COERCE, matching the
# exact shape in the roast regression.
role R2 {
    has Str:D $.attr is required;
    multi method new(Int:D $n) { self.new(attr => $n.Str) }
    multi method COERCE(Str:D $s) { R2.new(attr => $s) }
}

my $coerced = R2("hello");
is $coerced.attr, 'hello', 'COERCE-based construction works';
is $coerced.isa(R2.^pun), True, 'coerced instance isa its role pun';

my $newed = R2(42);
is $newed.attr, '42', 'Int-argument new() overload works';
is $newed.isa(R2.^pun), True, 'new()-built instance isa its role pun too';

done-testing;
