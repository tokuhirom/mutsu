use Test;

# An arithmetic operator whose Str operand cannot be numified evaluates to a
# lazy Failure (wrapping X::Str::Numeric) instead of dying on the spot, the way
# rakudo does. A module mainline that merely stores such a value must still
# load: WWW::HorizonsEphemerisSystem builds a table containing
# `'AngularDegrees' / 'Seconds'` at load time.

plan 16;

for (
    '+'  => { "a" + 1 },
    '-'  => { 1 - "b" },
    '*'  => { "a" * 2 },
    '/'  => { "a" / "b" },
    '%'  => { "a" % 2 },
    '**' => { "a" ** 2 },
    '×'  => { "a" × 2 },
    '÷'  => { "a" ÷ 2 },
) -> (:key($op), :value(&code)) {
    my $r = code();
    isa-ok $r, Failure, "infix:<$op> with a non-numeric Str is a Failure";
}

my %table = 'MeanMotion' => [['N'], 'AngularDegrees' / 'Seconds', 'Quantity'];
is %table<MeanMotion>.elems, 3, 'a Failure can be stored without being thrown';

my $f = "a" + "b";
is $f.exception.^name, 'X::Str::Numeric', 'Failure wraps X::Str::Numeric';
is $f.exception.source, 'a', 'the left operand is reported first';

throws-like { my $x = "a" + 1; $x + 1 }, X::Str::Numeric,
    'using the Failure in further arithmetic throws';
dies-ok { "a" + 1; 1 }, 'sinking the Failure throws';

isa-ok infix:<+>("a", 1), Failure, 'the routine form &infix:<+> agrees';
my $acc = "a";
$acc += 1;
isa-ok $acc, Failure, 'the assignment metaop stores the Failure';

is "3" + "4", 7, 'numeric strings still add';
