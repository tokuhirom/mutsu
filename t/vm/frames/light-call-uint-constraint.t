use Test;

# A `UInt` parameter or return type is one the light call paths check inline
# (`FastParamType::UInt`, #10110): a sign test for an `Int`, a big integer or
# a `Bool`, and the full type matcher for every other shape. This pins that a
# light call answers exactly what the general binder answers, including the
# rakudo wording of a rejected negative `Int` ("Constraint type check failed"),
# and that a user `multi infix:<*>(UInt, UInt --> UInt)` with `callsame` --
# FiniteField's hot path -- keeps working on it.

plan 20;

sub takes-uint(UInt $x --> UInt) { $x }
is takes-uint(3), 3, 'UInt parameter accepts a positive Int';
is takes-uint(0), 0, 'UInt parameter accepts zero';
is takes-uint(2 ** 70), 1180591620717411303424, 'UInt parameter accepts a big Int';
is takes-uint(True), True, 'UInt parameter accepts True';
is takes-uint(False), False, 'UInt parameter accepts False';
is takes-uint(Int).^name, 'Int', 'UInt parameter accepts the Int type object';
is takes-uint(UInt).^name, 'UInt', 'UInt parameter accepts the UInt type object';
is takes-uint(<5>).^name, 'IntStr', 'UInt parameter accepts a non-negative IntStr';

throws-like { takes-uint(-1) }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$x'; expected UInt but got Int (-1)",
    'a negative Int fails the UInt constraint';
throws-like { takes-uint(-2 ** 70) }, X::TypeCheck::Binding::Parameter,
    'a negative big Int fails the UInt constraint';
throws-like { takes-uint(<-3>) }, X::TypeCheck::Binding::Parameter,
    'a negative IntStr fails the UInt constraint';
my $rat = 1.0;
throws-like { takes-uint($rat) }, X::TypeCheck::Binding::Parameter,
    message => /'expected UInt but got Rat'/,
    'a Rat fails the UInt type check';

sub minus(UInt $a, UInt $b --> UInt) { $a - $b }
is minus(5, 3), 2, 'a UInt return passes a non-negative result';
throws-like { minus(3, 5) }, X::TypeCheck::Return,
    message => 'Type check failed for return value; expected UInt but got Int (-2)',
    'a UInt return rejects a negative result';

sub failing(--> UInt) { fail "nope" }
nok failing().defined, 'a Failure passes a UInt return check';

{
    my $*modulus = 1_000_003;
    multi infix:<*>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    multi infix:<+>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    my $acc = 1;
    for 1..200 -> $i { $acc = $acc * $i + $i }
    is $acc, 511005, 'UInt operator candidates with callsame compute modular arithmetic';
    is -2 * 3, -6, 'a negative operand falls through to the core candidate';
    is 2.5 * 2, 5, 'a Rat operand falls through to the core candidate';
    is (2 ** 70) * 1, (2 ** 70) mod 1_000_003, 'a big Int operand takes the UInt candidate';
}

{
    multi pick-kind(UInt $x) { 'uint' }
    multi pick-kind(Int $x) { 'int' }
    is pick-kind(4) ~ pick-kind(-4), 'uintint', 'a UInt multi candidate narrows over Int';
}
