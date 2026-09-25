use v6;
use Test;
use nqp;

# #9333: `IterationEnd` is a singleton, so `nqp::eqaddr` must see the bareword
# term and whatever `pull-one` returned as the same object. The standard
# nqp-level loop `nqp::until(nqp::eqaddr(($x := $it.pull-one), IterationEnd), ...)`
# used to never end (Hash::str, Iter::Able, String::Utils hung on it).
plan 16;

is nqp::eqaddr(IterationEnd, IterationEnd), 1, 'two bareword IterationEnd terms are the same object';
is nqp::eqaddr(Nil, Nil), 1, 'Nil is the same object as Nil';
my class C {};
is nqp::eqaddr(C, C), 1, 'a type object is the same object as itself';
my $e := IterationEnd;
is nqp::eqaddr($e, $e), 1, 'a bound IterationEnd is the same object as itself';
ok IterationEnd =:= IterationEnd, 'IterationEnd =:= IterationEnd';
is nqp::eqaddr("IterationEnd", IterationEnd), 0, 'a string with the same text is not the sentinel';

sub count-pulls(\it) {
    my Mu $x;
    my $n = 0;
    nqp::until(nqp::eqaddr(($x := it.pull-one), IterationEnd), $n++);
    $n
}

is count-pulls((1, 2).iterator), 2, 'List iterator';
is count-pulls((1..3).iterator), 3, 'Range iterator';
is count-pulls([1, 2, 3, 4].iterator), 4, 'Array iterator';
is count-pulls((1, 2).map(* * 2).iterator), 2, 'map Seq iterator';
is count-pulls((^5).grep(* %% 2).iterator), 3, 'grep Seq iterator';
is count-pulls({a => 1, b => 2}.iterator), 2, 'Hash iterator';
is count-pulls((gather { take 1; take 2 }).iterator), 2, 'gather iterator';
is count-pulls((1...4).iterator), 4, 'sequence operator iterator';

my class It does Iterator {
    has $.i = 0;
    method pull-one { $!i++ < 3 ?? $!i !! IterationEnd }
}
is count-pulls(It.new), 3, 'user Iterator returning the bareword IterationEnd';

sub typed(Int $a) {
    my $it := (1..$a).iterator;
    my Mu $x;
    my int $n = 0;
    nqp::until(nqp::eqaddr(($x := $it.pull-one), IterationEnd), $n++);
    $n
}
is typed(5), 5, 'the loop inside a statically typed routine';
