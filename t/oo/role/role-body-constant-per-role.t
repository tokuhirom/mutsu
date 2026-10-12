use Test;

# A role body's `my constant` belongs to that role: two roles may use the
# same name, and a parameterized candidate that composes the plain candidate
# (`role R[Int $e] does R`) still runs the plain candidate's body
# (Native::Packing's `my constant HostEndian = detect-host-endian()`).

plan 5;

role Q1 { my constant H = 1; method h { H } }
role Q2 { my constant H = 2; method h { H } }
class D1 does Q1 { }
class D2 does Q2 { }
is D1.new.h, 1, 'first role keeps its own constant';
is D2.new.h, 2, 'second role keeps its own constant';

my enum Pk::E <Net Vax Host>;
role Pk {
    sub det { Vax }
    my constant H = det();
    my $x = 5;
    method h { H }
    method x { $x }
}
role Pk[Pk::E $e] does Pk {
    method g { self.h }
}
class C does Pk[Vax] { }
is C.new.h, Vax, 'constant of the composed plain candidate';
is C.new.g, Vax, 'reached from the parameterized candidate';
is C.new.x, 5, 'a plain lexical of the plain candidate too';
