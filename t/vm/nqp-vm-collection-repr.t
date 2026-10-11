use v6;
use Test;
use nqp;

# A raw VMArray / VMHash (`nqp::list` / `nqp::hash`) is a different object from
# the List / Hash `nqp::hllize` makes of it, sharing the same element storage
# (ADR-12666). Every expected answer was checked against Rakudo 2026.09.
plan 64;

my $l := nqp::list(1, 2);
my $h := nqp::hash('x', 1);

# -- the raw objects and their types --
is $l.^name, 'BOOTArray', 'nqp::list is a BOOTArray';
is $h.^name, 'BOOTHash', 'nqp::hash is a BOOTHash';
is $l.REPR, 'VMArray', 'its REPR is VMArray';
is $h.REPR, 'VMHash', 'its REPR is VMHash';
is nqp::list_i(1).^name, 'BOOTIntArray', 'list_i is a BOOTIntArray';
is nqp::list_n(1e0).^name, 'BOOTNumArray', 'list_n is a BOOTNumArray';
is nqp::list_s('a').^name, 'BOOTStrArray', 'list_s is a BOOTStrArray';
is $l.HOW.^name, 'KnowHOW', 'a raw list has a KnowHOW';

# -- the BOOT type objects --
is nqp::bootarray().^name, 'BOOTArray', 'bootarray';
is nqp::boothash().^name, 'BOOTHash', 'boothash';
is nqp::bootintarray().^name, 'BOOTIntArray', 'bootintarray';
is nqp::bootnumarray().^name, 'BOOTNumArray', 'bootnumarray';
is nqp::bootstrarray().^name, 'BOOTStrArray', 'bootstrarray';
is nqp::bootarray().REPR, 'VMArray', 'bootarray REPR';
is nqp::boothash().REPR, 'VMHash', 'boothash REPR';
is nqp::eqaddr(nqp::what($l), nqp::bootarray()), 1, 'what(list) is bootarray';
is nqp::eqaddr(nqp::what(nqp::list_i(1)), nqp::bootintarray()), 1, 'what(list_i) is bootintarray';
is nqp::eqaddr(nqp::what($h), nqp::boothash()), 1, 'what(hash) is boothash';
is nqp::hlllist().^name, 'BOOTArray', 'hlllist';
is nqp::hllhash().^name, 'BOOTHash', 'hllhash';
is nqp::create(nqp::bootarray()).^name, 'BOOTArray', 'create(bootarray)';
is nqp::elems(nqp::create(nqp::bootarray())), 0, '... is empty';
{
    my $a := nqp::create(nqp::bootintarray());
    nqp::push_i($a, 3);
    is nqp::atpos_i($a, 0), 3, 'create(bootintarray) holds native ints';
}
is nqp::create(nqp::boothash()).^name, 'BOOTHash', 'create(boothash)';

# -- islist / ishash are REPR tests --
is nqp::islist($l), 1, 'islist: raw list';
is nqp::islist([1]), 0, 'islist: Array';
is nqp::islist((1, 2)), 0, 'islist: List';
is nqp::islist(slip(1, 2)), 0, 'islist: Slip';
is nqp::islist((1, 2).Seq), 0, 'islist: Seq';
is nqp::islist(nqp::list_i(1)), 1, 'islist: list_i';
is nqp::islist(nqp::list_s('a')), 1, 'islist: list_s';
{
    my int @i = 1, 2;
    is nqp::islist(@i), 1, 'islist: native array';
}
is nqp::islist(Buf.new(1)), 1, 'islist: Buf';
is nqp::islist(nqp::bootarray()), 1, 'islist: the BOOTArray type object';
is nqp::islist(nqp::bootintarray()), 1, 'islist: the BOOTIntArray type object';
is nqp::ishash($h), 1, 'ishash: raw hash';
is nqp::ishash({ a => 1 }), 0, 'ishash: Hash';
is nqp::ishash(Map.new), 0, 'ishash: Map';
is nqp::ishash(nqp::boothash()), 1, 'ishash: the BOOTHash type object';
is nqp::ishash($l), 0, 'ishash: a raw list';

# -- hllize: a different object over the same storage --
my $L := nqp::hllize($l);
my $H := nqp::hllize($h);
is $L.^name, 'List', 'hllize of a raw list is a List';
is $H.^name, 'Hash', 'hllize of a raw hash is a Hash';
is nqp::islist($L), 0, '... which is not a VMArray';
is nqp::ishash($H), 0, '... nor a VMHash';
is nqp::eqaddr($l, $L), 0, 'the raw list and its hllize are different objects';
is nqp::eqaddr($h, $H), 0, 'likewise for the hash';
is nqp::eqaddr(nqp::hllize($L), $L), 1, 'hllize of a high-level object is itself';
nqp::bindpos($l, 0, 99);
is $L[0], 99, 'bindpos on the raw list is visible through the List';
nqp::push($l, 3);
is $L.elems, 3, 'so is a push';
nqp::bindkey($h, 'y', 2);
is $H<y>, 2, 'bindkey on the raw hash is visible through the Hash';
is nqp::hllize(nqp::list_i(1)).^name, 'BOOTIntArray', 'a native raw array stays raw';
is nqp::islist(nqp::clone($l)), 1, 'clone of a raw list is raw';
is nqp::eqaddr(nqp::clone($l), $l), 0, '... and a new object';

# -- a raw value has no methods --
throws-like { $l.elems }, X::Method::NotFound, typename => 'BOOTArray',
    'a method call on a raw list dies';
throws-like { $h.keys }, X::Method::NotFound, typename => 'BOOTHash',
    'a method call on a raw hash dies';
is nqp::istype($l, Mu), 0, 'a raw list is not a Mu';
nok $l ~~ List, 'nor smartmatches List';

# -- binding a Raku parameter hllizes; assignment does not --
is (sub ($x) { nqp::islist($x) })($l), 0, 'a sub parameter';
is (-> $x { nqp::islist($x) })($l), 0, 'a pointy-block parameter';
is (sub ($x is raw) { nqp::islist($x) })($l), 0, 'an is raw parameter';
{
    my class C { method m($x) { nqp::islist($x) } }
    is C.m($l), 0, 'a method parameter';
}
{
    my $s = $l;
    is nqp::islist($s), 1, 'assignment to a scalar keeps it raw';
}
is nqp::islist(nqp::atpos(nqp::list($l), 0)), 1, 'nqp::atpos keeps it raw';
is (1, $l)[1].^name, 'List', 'a Raku subscript hands the element over hllized';
