use v6;
use Test;
use nqp;

# A List's `$!reified`, a Map's `$!storage` and a Capture's `@!list` /
# `%!hash` are raw VMArray / VMHash objects sharing the collection's storage,
# and `p6bindattrinvres` installs a raw one by sharing it (ADR-12666 §3.6,
# #12669). Every expected answer was checked against Rakudo 2026.09.
plan 34;

{
    my @a = 1, 2;
    my $r := nqp::getattr(@a, List, '$!reified');
    is nqp::islist($r), 1, '$!reified of an Array is a VMArray';
    is $r.^name, 'IterationBuffer', '... an IterationBuffer';
    is nqp::eqaddr($r, nqp::getattr(@a, List, '$!reified')), 1, '... the same object on every read';
    is nqp::eqaddr($r, @a), 0, '... and not the Array itself';
    nqp::push($r, 7);
    is @a.elems, 3, 'a push through it is seen by the Array';
    is @a[2], 7, '... with the pushed value';
}

{
    my %h = a => 1;
    my $s := nqp::getattr(%h, Map, '$!storage');
    is nqp::ishash($s), 1, '$!storage of a Hash is a VMHash';
    is $s.^name, 'BOOTHash', '... a BOOTHash';
    nqp::bindkey($s, 'b', 2);
    is %h<b>, 2, 'a bindkey through it is seen by the Hash';
}

{
    my %m := Map.new((a => 1));
    is nqp::ishash(nqp::getattr(%m, Map, '$!storage')), 1, '$!storage of a Map is a VMHash';
}

{
    my $raw := nqp::list(1);
    my $L := nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $raw);
    is $L.^name, 'List', 'p6bindattrinvres hands back the List';
    is $L.elems, 1, '... holding the raw list\'s elements';
    nqp::push($raw, 2);
    is $L.elems, 2, 'a later push through the raw list is seen by the List';
    is nqp::eqaddr(nqp::getattr($L, List, '$!reified'), $raw), 1,
        'its $!reified is the raw list itself';
}

{
    my $raw := nqp::list(1, 2);
    my @A := nqp::p6bindattrinvres(nqp::create(Array), List, '$!reified', $raw);
    is @A.^name, 'Array', 'an Array wrapper keeps its type';
    nqp::bindpos($raw, 0, 9);
    is @A[0], 9, '... and shares the raw storage';
}

{
    my $raw := nqp::hash('x', 1);
    my %H := nqp::p6bindattrinvres(nqp::create(Hash), Map, '$!storage', $raw);
    is %H.^name, 'Hash', 'a Hash wrapper over a raw hash';
    nqp::bindkey($raw, 'y', 2);
    is %H<y>, 2, '... shares the raw storage';
}

{
    my $raw := nqp::hash('x', 1);
    my %M := nqp::p6bindattrinvres(nqp::create(Map), Map, '$!storage', $raw);
    is %M.^name, 'Map', 'a Map wrapper keeps its type';
}

{
    sub f(|c) {
        (nqp::islist(nqp::getattr(c, Capture, '@!list')),
         nqp::ishash(nqp::getattr(c, Capture, '%!hash')))
    }
    my ($l, $h) = f(1, 2, a => 3);
    is $l, 1, 'a Capture\'s @!list is a VMArray';
    is $h, 1, 'a Capture\'s %!hash is a VMHash';
}

{
    # A wrapper's element data survives the round trip through hllize.
    my $raw := nqp::list(3, 4);
    my $L := nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $raw);
    is $L.join(','), '3,4', 'the adopted List reads its elements';
}

{
    # An Array's reified slots hold Scalar containers (#12698); a List's do
    # not. Checked against Rakudo.
    my @a = 1, 2;
    my $r := nqp::getattr(@a, List, '$!reified');
    is nqp::iscont(nqp::atpos($r, 0)), 1, 'an Array slot read through $!reified is a container';
    is nqp::atpos($r, 1), 2, '... that reads as the element';
    my $c := nqp::atpos($r, 0);
    $c = 5;
    is @a[0], 5, 'a write through the slot container reaches the Array';
    nqp::bindpos($r, 1, 7);
    is @a[1], 7, 'bindpos through $!reified still writes the Array';
    my $l := (1, 2);
    is nqp::iscont(nqp::atpos(nqp::getattr($l, List, '$!reified'), 0)), 0,
        'a List slot is a bare value';
}

{
    # bindpos installs the variable's container itself; an unassigned slot is
    # null (#12786). Checked against Rakudo.
    my @a = 1, 2;
    my $r := nqp::getattr(@a, List, '$!reified');
    my $s = 9;
    nqp::bindpos($r, 1, $s);
    $s = 10;
    is-deeply @a.List, (1, 10), 'bindpos of a variable shares its container with the Array';
    is nqp::iscont(nqp::atpos($r, 1)), 1, '... the slot is that container';
    my @b;
    @b[3] = 1;
    my $rb := nqp::getattr(@b, List, '$!reified');
    is nqp::isnull(nqp::atpos($rb, 0)), 1, 'an unassigned slot is null';
    is nqp::isnull(nqp::atpos($rb, 2)), 1, '... including the last gap';
    is nqp::isnull(nqp::atpos($rb, 3)), 0, '... but an assigned one is not';
    my $l := nqp::list;
    my $t = 1;
    nqp::bindpos($l, 0, $t);
    $t = 2;
    is nqp::atpos($l, 0), 2, 'bindpos of a variable into a raw list shares it too';
}

{
    # The $!reified view is an IterationBuffer, which derives from Any alone
    # (#12700). Checked against Rakudo.
    my @a = 1, 2;
    my $r := nqp::getattr(@a, List, '$!reified');
    is $r.^mro.map(*.^name).join(' '), 'IterationBuffer Any Mu', 'the view has IterationBuffer\'s MRO';
}
