# ADR-0115: a CORE type name used as an `nqp::` operand is folded to its type
# object at parse time, unless the compunit binds the name somewhere.
#
# Each case below names a CORE type in an `nqp::` operand. Where nothing binds
# the name, the operand is the CORE type object. Where the unit binds it (a
# sigilless parameter, a nested sigilless `my`, a user class declared later in
# the unit, an imported type), the operand must still mean what it binds, not
# CORE's. Every answer is checked against rakudo.
use nqp;
use Test;
use lib 't/lib';
use CoreNamedTypeExport;

plan 8;

my $b := nqp::create(IterationBuffer);
nqp::push($b, 1);
is nqp::elems($b), 1, 'nqp::create(IterationBuffer) makes an empty buffer';
ok nqp::istype($b, IterationBuffer), 'the folded operand is the CORE type';
ok nqp::istype([], List), 'List as an operand';

sub by-param(\Map) { nqp::istype(1, Map) }
ok by-param(Int), 'a sigilless parameter spelled like a CORE type is the parameter';

sub by-block() { my \Hash = Int; nqp::istype(1, Hash) }
ok by-block(), 'a nested sigilless binding shadows the CORE type';

sub by-class() { nqp::istype(Str.new, Str) }
ok by-class(), 'a class declared later in the unit is what the name means';
class Str { }

nok nqp::istype(1 => 2, Pair), 'an imported type spelled like a CORE one shadows it';

my $u := nqp::create(Uni);
nqp::push_i($u, 66);
is nqp::strfromcodes($u), 'B', 'nqp::create(Uni) makes an empty codepoint store';
