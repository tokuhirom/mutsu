use Test;

plan 5;

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
