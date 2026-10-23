use Test;

plan 9;

my class Foo { has $.s is rw = 3; has @.a = 1, 2; has %.h = a => 1 }
my $f = Foo.new;
my %attr = $f.^attributes(:local).map({ .name => $_ });

# `Attribute.get_value` answers the attribute's container; `.VAR` of it is the
# container, and its metamethods describe the container, not the value.
my \sv = %attr<$!s>.get_value($f).VAR;
is sv.^mro.map(*.^name).join(' '), 'Scalar Any Mu', '.VAR.^mro of a scalar attribute';
ok sv.^isa(Scalar), '.VAR.^isa(Scalar) through a sigilless alias';
ok %attr<$!s>.get_value($f).VAR.^isa(Scalar), '.VAR.^isa(Scalar) in a chain';
nok %attr<$!s>.get_value($f).VAR.^isa(Int), '.VAR.^isa(Int) is false';
ok %attr<@!a>.get_value($f).VAR.^isa(Array), 'array attribute';
ok %attr<%!h>.get_value($f).VAR.^isa(Hash), 'hash attribute';

# The same holds for an accessor result.
is $f.s.VAR.^mro.map(*.^name).join(' '), 'Scalar Any Mu', 'rw accessor .VAR.^mro';
ok $f.s.VAR.^isa(Scalar), 'rw accessor .VAR.^isa(Scalar)';
is $f.s.VAR.^name, 'Scalar', 'rw accessor .VAR.^name';
