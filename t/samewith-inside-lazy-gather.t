use lib $?FILE.IO.parent.add('lib').Str;
use Test;
use SamewithGatherModule;

plan 8;

# `samewith` is LEXICAL in Raku: it re-dispatches `&?ROUTINE`, the routine the
# call was written in. A lazy `gather` body runs after that routine has already
# returned, so a purely dynamic dispatch stack has nothing left to redispatch
# to. Digest::SHA3's `Keccak` ends in exactly this shape:
#
#     gather for samewith $inputBytes, :$delimitedSuffix, :$rate, :$capacity {...}

{
    proto K($x, $len?) {*}
    multi K($x)       { gather { take $x; take $x + 1 } }
    multi K($x, $len) { gather for samewith($x) { take $_ * $len } }
    is K(3, 10).list.join(","), "30,40",
        'samewith in a gather-for list resolves after the routine returned';
}

# The delegate need not itself be lazy.
{
    proto L($x, $len?) {*}
    multi L($x)       { (3, 4) }
    multi L($x, $len) { gather for samewith($x) { take $_ * $len } }
    is L(3, 10).list.join(","), "30,40",
        'samewith in a gather resolving to an eager candidate';
}

# A `take`-ing gather that calls samewith directly (not through a `for`).
{
    proto M($x, $len?) {*}
    multi M($x)       { $x * 2 }
    multi M($x, $len) { gather { take samewith($x); take $len } }
    is M(3, 10).list.join(","), "6,10", 'samewith directly inside a gather body';
}

# Methods keep their invocant.
{
    my class C {
        proto method m($x, $len?) {*}
        multi method m($x)       { gather { take $x; take $x + 1 } }
        multi method m($x, $len) { gather for self.m($x) { take $_ * $len } }
        multi method n($x)       { gather { take $x; take $x + 1 } }
        multi method n($x, $len) { gather for samewith($x) { take $_ * $len } }
    }
    is C.new.m(3, 10).list.join(","), "30,40", 'explicit self.m control case';
    is C.new.n(3, 10).list.join(","), "30,40", 'samewith in a gather inside a method';
}

# The capture is per-gather, not a global: a gather created in one routine and
# forced after a DIFFERENT routine has run redispatches its own routine.
{
    proto P($x, $len?) {*}
    multi P($x)       { gather { take $x; take $x + 1 } }
    multi P($x, $len) { gather for samewith($x) { take $_ * $len } }

    proto Q($x, $len?) {*}
    multi Q($x)       { gather { take $x * 100 } }
    multi Q($x, $len) { gather for samewith($x) { take $_ + $len } }

    my $p = P(3, 10);
    my $q = Q(3, 10);
    is $q.list.join(","), "310", 'the second gather redispatches its own routine';
    is $p.list.join(","), "30,40", 'and the first still redispatches its own';
}

# The Digest::SHA3 shape: the redispatched routine is module-private, and the
# gather is forced from the consumer's scope. Before the fix the dynamic stack
# named whichever routine was doing the forcing, so this redispatched the
# EXPORTED entry point with the inner routine's named arguments.
is SamewithGatherModule::hashit("abc"), "w:b(6,1088,512)w:b(6,1088,512)",
    'samewith in a gather resolves a module-private proto';
