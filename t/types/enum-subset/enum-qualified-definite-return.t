use Test;

plan 6;

enum P <A B C>;

# `--> Enum::Key` is a definite return of that enum value, like the bare `--> Key`.
class X { method p(--> P::B) {} }
is X.new.p, P::B, 'a method with an empty body returns the qualified enum value';
is X.new.p.^name, 'P', 'the value is an enum member';
sub f(--> P::C) { }
is f(), P::C, 'a sub with an empty body returns the qualified enum value';
sub g(--> P::A) { 42 }
is g(), P::A, 'the declared value replaces the body result';
sub h(--> C) { }
is h(), P::C, 'the bare key still works';
sub t(--> P) { P::A }
is t(), P::A, 'the enum type itself stays a type constraint';
