use Test;

# ADR-14843: a closure carries the frame record of the method that created
# it, so `self` and `::?CLASS` mean the creator's however and wherever it is
# called. Expected values are raku's.

plan 10;

class Maker {
    has $.name is required;
    method closure { -> { self.name } }
    method mapper(@xs) { @xs.map({ self.name ~ $_ }).join(',') }
    method gatherer { gather { take self.name; take self.name.uc } }
    method nested { my &f = { my &g = { self.name ~ '!' }; g() }; f() }
    method klass { -> { ::?CLASS.^name } }
}
class Other {
    has $.name = 'other';
    method run(&c) { c() }
    method call-it($c) { $c() ~ '/' ~ self.name }
}

my $a = Maker.new(name => 'alpha');
my $b = Maker.new(name => 'beta');
my $ca = $a.closure;
my $cb = $b.closure;

is $ca(), 'alpha', 'a closure keeps its creator as self';
is $cb(), 'beta', 'a second creator does not mix with the first';
is Other.new.run($ca), 'alpha', 'called under a different method it still answers its creator';
is Other.new.call-it($cb), 'beta/other', 'and the callee method keeps its own self';
is $a.mapper(<1 2>), 'alpha1,alpha2', 'map blocks see the method self';
is-deeply $b.gatherer.list, ('beta', 'BETA'), 'gather sees the method self';
is $a.nested, 'alpha!', 'a closure inside a closure keeps it';
is $a.klass.(), 'Maker', '::?CLASS in a closure is the creator class';

# after the creating call returned
sub make-late() { Maker.new(name => 'late').closure }
my $late = make-late();
is $late(), 'late', 'the closure outlives its creating frame';
is (1..3).map({ $a.name ~ $_ }).join, 'alpha1alpha2alpha3', 'a block outside any method reads the object';
