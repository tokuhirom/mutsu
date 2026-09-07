use v6;
use Test;

# raku reports the roles a class composes ITSELF in reverse declaration
# order -- the same last-wins rule a `but`-mixed role follows -- while a role
# reached THROUGH another one keeps its place after it. `.^mro(:roles)`,
# `.^roles` and `.^roles(:!transitive)` all agree.

plan 12;

role R1 { }
role R2 { }
role R3 { }

class One does R1 { }
class Two does R2 does R3 { }
class Three does R1 does R2 does R3 { }

role RA { }
role RB does RA { }
class Nest does RB { }

sub names($list) { $list.map({ .^name }).join(",") }

is names(Two.^mro(:roles)), "Two,R3,R2,Any,Mu",
    "two roles composed by one class are spliced last-declared-first";
is names(Two.^roles), "R3,R2", ".^roles uses the same order";
is names(Two.^roles(:!transitive)), "R3,R2", ".^roles(:!transitive) uses the same order";

is names(Three.^mro(:roles)), "Three,R3,R2,R1,Any,Mu",
    "three roles reverse all the way down";
is names(Three.^roles), "R3,R2,R1", "three roles reverse in .^roles too";

is names(One.^mro(:roles)), "One,R1,Any,Mu", "a single composed role is unaffected";
is names(One.^roles), "R1", "a single composed role is unaffected in .^roles";

is names(Nest.^mro(:roles)), "Nest,RB,RA,Any,Mu",
    "a role reached through another stays AFTER it, not reversed with it";
is names(Nest.^roles), "RB,RA", "the same in .^roles";
is names(Nest.^roles(:!transitive)), "RB",
    ":!transitive still reports only the direct composition";

# The plain `.^mro` must stay role-free, and the built-in seeds must keep the
# order rakudo reports for them -- they are not user declarations, so the
# last-declared-first flip does not apply to them.
is names(Two.^mro), "Two,Any,Mu", "the plain .^mro stays role-free";
is names(Array.^roles) ~ " " ~ names(Hash.^roles),
    "Positional,Iterable Associative,Iterable",
    "built-in role lists keep their reported order";
