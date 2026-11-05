use v6;
use Test;

# From eigenstates: a parameter typed `Junction:D` / `Mu:D` binds a Junction
# whole, like the bare `Junction` / `Mu`; a closure called by value must not
# autothread it.

plan 8;

my &anon-sub = sub (Junction:D $j) { "J" };
is anon-sub(1|2), "J", 'anonymous sub with Junction:D';

my &pointy = -> Junction:D $j { "J" };
is pointy(1|2), "J", 'pointy block with Junction:D';

my &mu-def = -> Mu:D $m { "M" };
is mu-def(1|2), "M", 'pointy block with Mu:D';

my &bare = -> Junction $j { "J" };
is bare(1|2), "J", 'bare Junction already worked';

my &threaded = -> Int:D $i { $i * 2 };
ok threaded(1|2) ~~ Junction, 'an Int:D parameter still threads';
ok so threaded(1|2) == 4, 'and the threaded result holds each eigenstate';

{
    my proto sub es(|) {*}
    my multi sub es(Junction:D $j) { "J" }
    my multi sub es(Mu \o) { "M" }
    my &c = &es;
    is c(1|2), "J", 'captured multi family takes the Junction whole';
    is c(42), "M", 'and a plain value takes the other candidate';
}
