use Test;

plan 6;

# An `is rw` method named apart from the attribute it returns (through
# `nqp::getattr`) takes element assignment into that attribute (#17476).

use nqp;

class O { has %.map; has @.list; }
O.^add_method('m1', my method () is rw { nqp::getattr(self, O, '%!map') });
O.^add_method('l1', my method () is rw { nqp::getattr(self, O, '@!list') });
O.^compose;

my $o = O.new;
$o.m1<x> = 5;
is-deeply $o.map, {x => 5}, 'hash element through an added rw method';
$o.m1<y> = 6;
is-deeply $o.map, {x => 5, y => 6}, 'a second element';
$o.l1[1] = 'b';
is-deeply $o.list, [Any, 'b'], 'array element through an added rw method';

class P {
    has %!h;
    method hh() is rw { nqp::getattr(self, P, '%!h') }
    method dump { %!h }
}
my $p = P.new;
$p.hh<y> = 1;
is-deeply $p.dump, {y => 1}, 'private hash through a differently named rw method';
$p.hh<z> = 2;
is $p.dump.elems, 2, 'and again';
is $p.hh<z>, 2, 'read back through the method';
