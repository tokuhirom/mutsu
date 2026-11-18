use Test;
use nqp;

# `$!attr := VALUE` seats the value itself as the attribute, so a later
# method's `nqp::eqaddr($!attr, $v)` / `=:=` compares the object, not a
# Scalar (#18024: ParaSeq's `$!lastsema` check never matched, so a `last`
# in a hyperized map block did not stop the result).

plan 10;

my class PQ is repr('ConcBlockingQueue') { }

class Holder {
    has $!last;
    method set($s is raw) { $!last := $s unless nqp::isconcrete($!last) }
    method same($n is raw) { nqp::eqaddr($n, $!last) }
    method same-flipped($n is raw) { nqp::eqaddr($!last, $n) }
    method ident($n is raw) { $!last =:= $n }
}

my $q  := nqp::create(PQ);
my $q2 := nqp::create(PQ);
my $h = Holder.new;
$h.set($q);
ok  $h.same($q),         'eqaddr sees the object bound in another method';
ok  $h.same-flipped($q), 'eqaddr with the attribute on the left';
nok $h.same($q2),        'a different object is not the same';
ok  $h.ident($q),        '=:= agrees';

my $ib := nqp::create(IterationBuffer);
my $h2 = Holder.new;
$h2.set($ib);
ok $h2.same($ib), 'an IterationBuffer bound into an attribute';

my $p := Promise.new;
my $h3 = Holder.new;
$h3.set($p);
ok $h3.same($p), 'a Promise bound into an attribute';

# The object travels through a queue to another thread and back.
my $outer := nqp::create(PQ);
nqp::push($outer, $q);
ok (await start { $h.same(nqp::shift($outer)) }), 'compared on another thread';

# An assigned attribute owns a Scalar, as on rakudo.
class Assigned {
    has $!v;
    method set($s) { $!v = $s }
    method same($n is raw) { nqp::eqaddr($!v, $n) }
}
my $a = Assigned.new;
$a.set($p);
nok $a.same($p), 'an assigned attribute is a Scalar, not the object';

# Two names bound to one object.
my $r := $p;
ok $r =:= $p, 'two bound names of one Promise are =:=';
my $m = $p;
nok $m =:= $p, 'an assigned name owns its own Scalar';
