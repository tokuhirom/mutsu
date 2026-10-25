use lib 't/lib';
use Test;

# A trait of a DECLARE'd class that mixes a parameterised role into the class's
# HOW (`$type.HOW does Carries[$arg]`) must still carry its role argument when
# the class is declared as a term (`my \v = vessel V is carries(Payload) { }`):
# the in-place registration keeps the metaobject the compile-time shell's trait
# produced instead of re-deriving it from the mixin's bare role names.

use DeclareTraitMixin;

plan 6;

vessel Statement is carries(Payload) { }
is Statement.^carried.^name, "Payload", "statement form: the role argument reaches the method";

my \term = vessel Term is carries(Payload) { };
is term.^carried.^name, "Payload", "term form: the role argument reaches the method";
is term.HOW.^name, 'TraitMixinHOW+{Carries[Payload]}', "term form: the HOW is the parameterised mixin";

given vessel Given is carries(Payload) { } {
    is .^carried.^name, "Payload", "given form";
}

my \plain = vessel Plain { };
is plain.HOW.^name, "TraitMixinHOW", "a term-form declaration without the trait is untouched";

sub make { vessel InSub is carries(Payload) { } }
is make().^carried.^name, "Payload", "a term-form declaration in a sub body";
