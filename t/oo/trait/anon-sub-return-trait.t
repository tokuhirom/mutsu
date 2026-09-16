use Test;

plan 4;

# Function::Validation exposed that an anonymous sub's `returns` trait was
# parsed but discarded before its runtime Signature was built.
my $typed = sub (Int $count, Str $text) returns Str {
    $text x $count
}

is $typed.signature.returns.^name, 'Str',
    'an anonymous sub preserves a trait return type in its Signature';
is $typed(2, 'x'), 'xx', 'the anonymous sub still returns its value';

my $wrong = sub (Int $count, Str $text) returns Int {
    $text x $count
}
dies-ok { $wrong(1, 'x') },
    'an anonymous sub enforces a trait return type';

ok $typed.signature.returns === Str,
    'the anonymous sub Signature contains the Str type object';
