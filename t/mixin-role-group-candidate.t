use v6;
use Test;

# Declaring one role name twice with different parameter lists forms a role
# GROUP. `1 but Z[Str]` must select the same candidate `class C does Z[Str]`
# does -- the mixin path used to compose whichever candidate registered last
# for the bare name.

plan 16;

role Z { has Int $.n }
role Z[::T] { has $.a = T }

is (1 but Z[Str]).a.gist, "(Str)", "the parameterised candidate's attribute exists";
is (1 but Z).n.gist, "(Int)", "the unparameterised candidate still composes";
is (1 but Z[Str]).^name, 'Int+{Z[Str]}', "the composed name carries the arguments";
is (1 but Z).^name, 'Int+{Z}', "the bare composition keeps the bare name";
ok (1 but Z[Str]).does(Z[Str]), ".does agrees with the composed arguments";
nok (1 but Z[Str]).does(Z[Int]), ".does rejects different arguments";
is Z[Str].new.a.gist, "(Str)", "the role-punning path is unaffected";

# Candidates that differ in ARITY: the name's single recorded parameter list
# cannot describe both, so the selected candidate's own list has to be used.
role W[::T] { method w { "1:" ~ T.^name } }
role W[::T, ::U] { method w { "2:" ~ T.^name ~ U.^name } }

is (1 but W[Str]).w, "1:Str", "the one-argument candidate binds its own parameter";
is (1 but W[Str, Int]).w, "2:StrInt", "the two-argument candidate binds both of its own";
is (1 but W[Str]).^name, 'Int+{W[Str]}', "one-argument composed name";
is (1 but W[Str, Int]).^name, 'Int+{W[Str,Int]}', "two-argument composed name";

# A candidate selected by a `where` constraint on the parameter.
role P[$n where * > 3] { method p { "big$n" } }
role P[$n] { method p { "small$n" } }

is (1 but P[5]).p, "big5", "a where-constrained candidate is selected when it matches";
is (1 but P[2]).p, "small2", "and passed over when it does not";

# Two compositions of the same candidate share a type; two different
# candidates of one group do not.
role Q { has $.q = "bare" }
role Q[::T] { has $.q = "param" }

ok (1 but Q[Str]).WHAT =:= (1 but Q[Str]).WHAT, "same candidate, same composed type";
nok (1 but Q[Str]).WHAT =:= (1 but Q).WHAT, "different candidates, different composed types";

# An initialisation argument is not a type argument, so it still reaches the
# single-public-attribute path rather than candidate resolution.
role I { has $.i }
is (1 but I(42)).i, 42, "an initialiser argument is unaffected";
