use Test;

# From the RPi::Device::ST7036 ecosystem distribution: `v33 => False` is a
# named argument / pair key, not the version literal v33.

plan 6;

class A { has Bool $.v33 is required; has $.x; }
is A.new(v33 => False, x => 1).v33, False, 'v33 => ... is a named argument';

my %h = v33 => 1, v6 => 2;
is-deeply %h.sort.map(*.key).List, ('v33', 'v6'), 'version-like bareword keys in a hash';

sub f(:$v1) { $v1 }
is f(v1 => 5), 5, 'v1 => 5 binds :$v1';
is (v1 => 3).key, 'v1', 'pair key is the string v1';
is (v1.2.3).Str, '1.2.3', 'a real version literal still parses';
is (v6.c ~~ v6.c), True, 'v6.c still a version';
