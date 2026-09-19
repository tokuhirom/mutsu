use Test;

plan 2;

use MONKEY-SEE-NO-EVAL;

# TimeBomb 0.0.1 redeclares wrapped routines through EVAL and must not inherit
# the previous declaration's named wrap chain.
try {
    EVAL(q[sub foo() { 42 }; &foo.wrap(-> | { die "old wrapper" }); foo]);
}
is $!.Str, "old wrapper", "the original wrapped routine dies through its wrapper";

is EVAL(q[sub foo() { 43 }; foo]), 43,
    "an EVAL redeclaration does not inherit the old routine's wrap chain";
