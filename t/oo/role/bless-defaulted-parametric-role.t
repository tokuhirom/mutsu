use Test;

plan 3;

# #11652: `self.bless` inside a role's own `method new`, called on a defaulted
# parametric role, binds the role's parameter defaults for attribute defaults.
role R[$f = 5] { has $.c = $f; method new { self.bless } }
is R.new.c, 5, 'bless on a defaulted parametric role binds the default';
is R[7].new.c, 7, 'explicit parameterisation still binds its argument';

role S[$f = 5] { has $.c = $f }
is S.new.c, 5, 'plain .new on a defaulted parametric role (control)';
