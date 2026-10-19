# For t/routines/routine-scoped-use-rolls-back-each-call.t
unit module RoutineScopedUseFixture;
sub rsuf-greet() is export { 'hi' }
multi sub rsuf-m(Int $x) is export { "int $x" }
multi sub rsuf-m(Str $x) is export { "str $x" }
