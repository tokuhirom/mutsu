use Test;
use nqp;

plan 17;

class HLLBoundary {
    method items { nqp::list(1, 2, 3) }
    method raw-items is raw { nqp::list(4, 5) }
    method table { nqp::hash('answer', 42) }
    method integer { nqp::box_i(42, nqp::bootint()) }
    method number { nqp::box_n(2.5e0, nqp::bootnum()) }
    method string { nqp::box_s('hello', nqp::bootstr()) }
    method native-items { nqp::list_i(7, 8) }
}
sub raw-items { nqp::list(9, 10) }

is HLLBoundary.items.^name, 'List', 'type-object method result is HLL-ized';
my $count = 0;
$count++ for HLLBoundary.items;
is $count, 3, 'a method returning an NQP list can be iterated';
is HLLBoundary.raw-items.^name, 'List', 'is raw does not suppress the HLL boundary';
is HLLBoundary.table.^name, 'Hash', 'raw hash result becomes Hash';
is HLLBoundary.table<answer>, 42, 'hash result retains its entries';
is HLLBoundary.integer.^name, 'Int', 'BOOTInt result becomes Int';
is HLLBoundary.integer, 42, 'boxed integer retains its value';
is HLLBoundary.number.^name, 'Num', 'BOOTNum result becomes Num';
is HLLBoundary.string, 'hello', 'BOOTStr result becomes Str';
is HLLBoundary.string.^name, 'Str', 'boxed string has HLL type';
is HLLBoundary.native-items.^name, 'BOOTIntArray', 'native arrays stay raw under hllize';
is raw-items().^name, 'BOOTArray', 'subroutine return stays raw';
my $object = HLLBoundary.new;
is $object.items.^name, 'List', 'named receiver result is HLL-ized';
my $method = 'items';
is $object."$method"().^name, 'List', 'dynamic name on named receiver is HLL-ized';
is HLLBoundary."$method"().^name, 'List', 'dynamic name on expression receiver is HLL-ized';
my $raw := nqp::list(11, 12);
class SharedBoundary {
    method items { $raw }
}
my $list := SharedBoundary.items;
nqp::push($raw, 13);
is $list.elems, 3, 'HLL conversion shares the original raw storage';
is nqp::islist($raw), 1, 'conversion leaves the original raw value raw';
