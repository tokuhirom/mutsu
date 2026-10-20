use v6;
use Test;

plan 3;

# `unit module A::Reg; class Reg {}`: the compunit's own package is not a
# nested package, so `use A::Reg` must not publish it as `Reg`, where it hid a
# later `constant Reg = A::Reg::Reg` in a return type (#14086).
use lib 't/lib';
use Shadow14086::Reg;

constant Reg = Shadow14086::Reg::Reg;
my $r = Reg.new(x => 1);
sub f(--> Reg) { $r }

is f().^name, 'Shadow14086::Reg::Reg', 'the constant alias wins in a return type';
is Reg.^name, 'Shadow14086::Reg::Reg', 'the short name is the constant';
isa-ok f(), Reg, 'the returned object is the class';
