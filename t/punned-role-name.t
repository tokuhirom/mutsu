use Test;

# A punned role (`R.new`) builds `Mixin(Instance{R}, {__mutsu_role__R})` — the
# role composed onto its OWN same-named (empty) instance, not a mixin onto a
# different base. The role-suffixed `.^name` must therefore report plain `R`, not
# `R+{R}`: only a role mixed onto a DIFFERENT base is suffixed. (zef's
# `Zef::Candidate` is a punned role, and reported `Zef::Candidate+{Zef::Candidate}`.)

plan 6;

role R { has $.a; }
class W { has $.id; }
class C does R {}

is R.new(a => 1).^name, 'R',
    'a punned role instance reports the plain role name, not R+{R}';
is R.^name, 'R', 'the role type object reports the plain role name';
is C.new(a => 1).^name, 'C', 'a class that does a role reports the class name';
is (W.new(id => 1) but R).^name, 'W+{R}',
    'a role mixed onto a DIFFERENT base keeps the +{Role} suffix';

# A punned role that also has another role mixed in keeps only the other role in
# the suffix (the pun itself is not repeated).
role R2 { has $.b; }
is (R.new(a => 1) but R2).^name, 'R+{R2}',
    'a punned role with an extra mixin suffixes only the extra role';

# The .WHAT of a punned role round-trips to the same name.
is R.new(a => 1).WHAT.^name, 'R', 'WHAT of a punned role instance is the role';
