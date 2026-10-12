use Test;

# Attribute.get_value / set_value on an object mixed in with `but`
# (Native::Packing's `$c but Native::Packing[Vax]` then reads every attribute).

plan 5;

role R { }
class C { has int16 $.a; has $.b }

my $m = C.new(:a(42), :b(7)) but R;
my %seen = $m.^attributes.map({ .name => .get_value($m) });
is %seen<$!a>, 42, 'get_value reads a native attribute through the mixin';
is %seen<$!b>, 7, 'get_value reads a plain attribute through the mixin';

for $m.^attributes.grep(*.name eq '$!b') { .set_value($m, 9) }
is $m.b, 9, 'set_value writes through the mixin';
is $m.a, 42, 'the other attribute is untouched';
is C.new(:a(1)).^attributes[0].get_value(C.new(:a(5))), 5, 'plain objects still work';
