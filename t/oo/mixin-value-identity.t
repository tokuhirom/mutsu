use v6;
use Test;

# `===` is `.WHICH eq .WHICH`: the BASE value's identity plus the composed
# type. So two separately-built but identically-composed values ARE identical:
#
#     role A { }
#     (1 but A) === (1 but A)   # True
#
# mutsu compared the two `Mixin` values' raw `overrides` maps, and every role
# application stamps its own `__mutsu_role_seq__{name}` (the monotonic
# application-order bookkeeping that makes later-applied roles win a method
# collision) — so the answer could never be True by construction.
#
# `mixin_identity_key` drops that stamp (keeping only the ORDER it encodes) and
# the per-instance `__mutsu_attr__*` values, and keeps everything else. Every
# expectation below was measured against rakudo 2026.07.

plan 18;

role A { }
role B { }
role R { has $.x }
role C { method m { 1 } }

# --- the ticket's repro ------------------------------------------------
ok (1 but A) === (1 but A), 'two identical role mixins over a value type are identical';
ok ("s" but A) === ("s" but A), 'the same over a Str';
ok (1 but A) eqv (1 but A), 'and they are eqv';
ok (1 but C) === (1 but C), 'a role carrying a method too';

# The composition key was already right — this is what narrowed it to `===`.
ok (1 but A).WHAT =:= (1 but A).WHAT, 'the composed .WHAT was already shared';

# --- what must stay apart ---------------------------------------------
nok (1 but A) === (1 but B), 'two DIFFERENT roles are not identical';
nok (1 but A) === 1, 'a mixed value is not identical to its base';
nok (1 but "x") === (1 but "x"), 'a `but <non-role>` mints a fresh anonymous type each time';

# A reference-type base is not identical even when equal, mixin or not.
nok ([1, 2] but A) === ([1, 2] but A), 'two equal Arrays under the same role are NOT identical';
my $arr = [1, 2] but A;
ok $arr === $arr, 'but one Array under a role is identical to itself';

# Application ORDER is part of the composed type.
ok ((1 but A) but C) === ((1 but A) but C), 'the same two roles in the same order are identical';
nok ((1 but A) but C) === ((1 but C) but A), 'the same two roles in the OTHER order are not';

# --- per-instance role attributes are NOT part of identity -------------
ok (1 but R(2)) === (1 but R(2)), 'a role with an attribute, same initialiser';
ok (1 but R(2)) === (1 but R(3)), 'and a DIFFERENT initialiser is still identical';

# --- allomorphs, which share the `Mixin` representation ----------------
ok <42> === <42>, 'two allomorphs with the same parts are identical';
nok <42> === IntStr.new(42, "forty-two"), 'a different string part is not';
nok <42> === 42, 'and an allomorph is not its numeric part';

# --- the base value still decides ---------------------------------------
nok (1 but A) === (2 but A), 'the same role over different base values is not identical';
