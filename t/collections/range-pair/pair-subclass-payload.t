use Test;

# A user subclass of Pair (ValuePair from the ecosystem `immutable` dist's
# dependency closure): construction, the Pair accessors, and the qualified
# `self.Pair::new(...)` call from the subclass's own `new`.

plan 14;

class Plain is Pair { method hi { "hi" } }
my $p = Plain.new("k", 3);
isa-ok $p, Plain, 'positional new builds the subclass';
ok $p ~~ Pair, 'subclass instance is a Pair';
is $p.key, "k", '.key';
is $p.value, 3, '.value';
is $p.hi, "hi", 'subclass method';
is $p.raku, ':k(3)', '.raku';
is $p.gist, 'k => 3', '.gist';

my $n = Plain.new(key => "a", value => 4);
is $n.kv.join(","), "a,4", 'named key/value constructor';

class VP is Pair {
    proto method new(|) {*}
    multi method new(Pair:D $pair) { self.Pair::new($pair.key, $pair.value) }
    multi method new($key, $value) { self.Pair::new($key, $value) }
    multi method new(:$key!, :$value!) { self.Pair::new($key, $value) }
    multi method raku(VP:D:) { self.^name ~ '.new(' ~ self.key.raku ~ ',' ~ self.value.raku ~ ')' }
}
my $v = VP.new("a", 42);
isa-ok $v, VP, 'self.Pair::new keeps the subclass type';
is $v.raku, 'VP.new("a",42)', 'user raku override wins';
is VP.new(key => "b", value => 1).value, 1, 'named multi';
is VP.new((:x(5))).key, "x", 'Pair multi';
is VP.new("a", 42).Str, "a\t42", '.Str delegates';
is $v.antipair.key, 42, '.antipair delegates';

