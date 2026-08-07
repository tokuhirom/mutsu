use Test;

plan 7;

my constant cname = 'LocalIndirectClass';
class ::(cname) {
    method value() { 42 }
}
is LocalIndirectClass.value, 42, 'class ::(name) declares a type with the resolved name';

my constant sname = 'indirect_sub';
sub ::(sname) ($x) { $x + 1 }
is indirect_sub(41), 42, 'sub ::(name) declares a callable sub';
is &indirect_sub.name, 'indirect_sub', 'indirectly named sub reports resolved name';

class M {
    method ::('sp ace') { 23 }
}
is M."sp ace"(), 23, 'method ::(\"...\") allows method names with spaces';

ok ::('&say') =:= &say, '::(\"&foo\") resolves code objects via indirect lookup';

class MultiIndirect {
    my constant mname1 = 'second';
    my constant mname2 = 'fourth';
    method first() { 1 }
    method ::(mname1) { 2 }
    method third() { 3 }
    method ::(mname2) { 4 }
}
is-deeply
    (MultiIndirect.first, MultiIndirect.second, MultiIndirect.third, MultiIndirect.fourth),
    (1, 2, 3, 4),
    'multiple ::(...) methods interleaved with ordinary methods resolve correctly';

role RIndirect {
    my constant rname = 'rsecond';
    method rfirst() { 'a' }
    method ::(rname) { 'b' }
    method rthird() { 'c' }
}
class UsesRIndirect does RIndirect { }
is-deeply
    (UsesRIndirect.rfirst, UsesRIndirect.rsecond, UsesRIndirect.rthird),
    ('a', 'b', 'c'),
    'role ::(...) methods interleaved with ordinary methods resolve correctly';
