use Test;

# Found via FontFactory::Type1 (Font::AFM's `my ::?CLASS:U $class = ...`).
# `::?CLASS` in a variable declaration names the enclosing class; it is not an
# unbound type capture (which would type the variable NQPMu).

plan 8;

class Base {
    method mk(Str $n, |c --> Base:D) {
        my ::?CLASS:U $class = self.cls($n);
        $class.new: |c;
    }
    method cls($n) { ::($n) }
}
class Derived is Base { has $.x = 5 }

my Base $a .= mk("Derived");
isa-ok $a, Derived, 'a subclass type object assigned to a ::?CLASS:U variable';
is $a.x, 5, 'and instantiated';

my Base $b;
$b .= mk("Derived");
isa-ok $b, Derived, 'same through a typed container';

class Probe {
    method plain  { my ::?CLASS $c = Probe; $c.^name }
    method undef  { my ::?CLASS:U $c = Probe; $c.^name }
    method defd   { my ::?CLASS:D $c = self.new; $c.^name }
    method assign { my ::?CLASS:U $c; $c = Probe; $c.^name }
}
is Probe.plain, 'Probe', 'plain ::?CLASS declaration';
is Probe.undef, 'Probe', ':U smiley';
is Probe.defd, 'Probe', ':D smiley';
is Probe.assign, 'Probe', 'declare then assign';

role R { method r { my ::?CLASS:U $c = ::?CLASS; $c.^name } }
class Consumer does R { }
is Consumer.r, 'Consumer', 'inside a role method, ::?CLASS is the consuming class';
