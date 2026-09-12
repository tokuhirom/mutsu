use Test;

plan 8;

my sub return-two(--> 2) { 3 }
is return-two(), 2, 'definite return value overrides implicit final expression';

throws-like
    'my sub bad(--> Nil) { return 1 }',
    X::Comp,
    'return with a value is rejected for definite return specs',
    payload => /Nil/;

my $sunk = False;
my sub return-empty(--> Empty) { 1, { ++$sunk; last } ... * }
is return-empty().elems, 0, 'definite Empty return yields an empty list';
ok $sunk, 'final expression still runs in sink context';

my $pointy = -> --> "done" { 42 };
is $pointy(), "done", 'pointy blocks accept definite return values';

# #8022: a `-->` spec naming an ENUM VALUE (not its type) is a definite
# return of that value, regardless of the body -- exactly like `--> 42` or
# `--> "lit"`. The enum's own TYPE name must still be a type constraint.
enum E8022 <A8022 B8022 C8022>;
my sub definite-enum-value($x --> B8022) { }
is definite-enum-value(1), B8022, 'a `-->` spec naming an enum VALUE returns that value';

my sub enum-type-constraint($x --> E8022) { $x }
is enum-type-constraint(A8022), A8022,
    "the enum's own TYPE name in `-->` stays a type constraint, not a value";

throws-like
    { enum-type-constraint(5) },
    X::TypeCheck::Return,
    'an enum TYPE constraint in `-->` still rejects a mismatched return value';
