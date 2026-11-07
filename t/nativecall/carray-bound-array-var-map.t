use Test;
use NativeCall;

# Found through Math::FFT::Libfftw3 (C2C.execute): `@!out := CArray[num64].new: ...`
# then `@!out.map(-> $r, $i { ... })`. A `.map` written on an `@` variable bound to
# a CArray saw the CArray as a single item.

plan 7;

my @y := CArray[num64].new: 1e0, 2e0, 3e0, 4e0;
is @y.map(* + 1).list, (2, 3, 4, 5), 'map over an @ variable bound to a CArray';
is @y.map(-> $r, $i { $r + $i }).list, (3, 7), 'a two-argument pointy block takes two elements';
is @y.grep(* > 2).list, (3, 4), 'grep agrees';
is @y.elems, 4, 'elems';

class C {
    has num64 @.out;
    method pairs-sum {
        @!out := CArray[num64].new: 1e0, 2e0, 3e0, 4e0;
        @!out.map(-> $r, $i { $r + $i }).list
    }
}
is C.new.pairs-sum, (3, 7), 'map over a typed attribute bound to a CArray';

my $s := @y;
is $s.map(-> $r, $i { $r * $i }).list, (2, 12), 'a scalar bound to the same CArray';
my @plain = 1e0, 2e0, 3e0, 4e0;
is @plain.map(-> $r, $i { $r + $i }).list, (3, 7), 'a plain array still maps';
