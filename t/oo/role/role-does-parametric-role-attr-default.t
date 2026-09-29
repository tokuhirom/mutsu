use v6;
use Test;

# A parametric role's attribute default used to be lost when the role was
# reached through another, non-parametric role instead of being composed
# directly: `role Kg does U["g"] {}` records U's bound type parameter under
# "Kg" (there being no class yet to key it by), but nothing carried that
# binding forward when a class or mixin later composed Kg -- the parameter
# stayed unbound and the attribute default evaluated against Any (#9834).

plan 4;

role U[$unit] { has $.sym = $unit; }
role Kg does U["g"] {}

class D does Kg {}
is D.new.sym, 'g', 'attribute default reaches a class through an intermediate role';

is (5 does Kg).sym, 'g', 'attribute default reaches a mixin through an intermediate role';

# Direct composition (no intermediate role) must keep working.
class C does U["x"] {}
is C.new.sym, 'x', 'attribute default still works composing the parametric role directly';

is (5 but U["y"]).sym, 'y', 'attribute default still works mixing the parametric role directly';
