use Test;

plan 11;

# ADR-0019 Phase E box E7 step 7 (`.WALK`, `todo/deep/
# adr0019-e5-e7-entry-routing.md` "E7 step 7"): `.WALK` must also accept a
# runtime mixin (`$obj but Role`) as its receiver, visiting the mixed-in
# role's OWN methods ahead of the base class chain -- real raku puts a
# mixin's anonymous pun class FIRST in `.^mro` (`(WSub.new but R1).^mro` is
# `(WSub+{R1}) WSub WBase Any Mu`), so `R1`'s methods must be found even
# though they are never composed into any class's method table. Before this
# fix, WALK did not recognize a Mixin receiver at all and raised
# X::Method::NotFound.

role R1 { method zork { "R1::zork" } }
role R2 { method quux { "R2::quux" } }
class WBase { method foo { "WBase::foo" } }
class WSub is WBase { method foo { "WSub::foo" } }

# A role mixed onto a plain (non-instance) builtin value.
my $i = 5 but R1;
is $i.WALK("zork")().Str, 'R1::zork', 'WALK finds a role mixed onto a builtin Int';

# A role mixed onto a user-class instance: the base class chain is still
# walkable through the mixin ...
my $x = WSub.new but R1;
is $x.WALK("foo")().Str, 'WSub::foo WBase::foo',
  'WALK still walks the base class chain through a mixin';
# ... and the mixed-in role's own method is found too, even though it is not
# composed into WSub's or WBase's method table.
is $x.WALK("zork")().Str, 'R1::zork', 'WALK finds the mixed-in role method too';
# A method neither the base chain nor the mixin has is still unmatched.
is $x.WALK("quux")().elems, 0, 'WALK finds nothing for an absent method name';

# Two roles mixed onto the same value: each contributes its own method
# independently.
my $y = (WSub.new but R1) but R2;
is $y.WALK("zork")().Str, 'R1::zork', 'first mixed-in role of a two-role stack';
is $y.WALK("quux")().Str, 'R2::quux', 'second mixed-in role of a two-role stack';
is $y.WALK("foo")().Str, 'WSub::foo WBase::foo',
  'base class chain is unaffected by a two-role mixin stack';

# A mixin role's own method wins ordinary (non-WALK) qualified/unqualified
# dispatch too -- WALK's mixin-role candidates must agree with this, not
# introduce a second answer.
class WOverride { method foo { "WOverride::foo" } }
role ROverride { method foo { "ROverride::foo" } }
my $z = WOverride.new but ROverride;
is $z.foo, 'ROverride::foo', 'sanity: the mixin itself wins ordinary dispatch';
is $z.WALK("foo")().Str, 'ROverride::foo WOverride::foo',
  'WALK visits the winning mixin candidate before the shadowed base one';

# Attribute access through a mixin-wrapped instance's own (base-class) WALK
# candidate must reach the live instance, not an empty snapshot.
class WAttr { has $.n = 42; method get { $!n } }
my $w = WAttr.new but R1;
is $w.WALK("get")().Str, '42', 'a mixin-wrapped base-class candidate sees the instance attrs';

# A builtin receiver with no user class_def and no mixin still uses the
# pre-existing builtin-type WALK table (regression guard for the branch this
# fix narrowed with `mixin_role_names.is_empty()`).
grammar GBase { token TOP { . } }
ok GBase.WALK(:name<parse>).elems >= 1, 'a plain (non-mixin) builtin-type WALK receiver is unaffected';
