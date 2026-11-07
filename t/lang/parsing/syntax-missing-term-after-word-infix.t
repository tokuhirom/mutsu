use Test;

plan 14;

# The operators whose operand parser has no "after X operator" message of its
# own also fail with rakudo's X::Comp::AdHoc "Missing required term after
# infix" (#16976, the part #16986 left on the generic error).
sub failure($code) {
    try { EVAL $code; CATCH { default { return .^name ~ ': ' ~ .message } } }
    'no error';
}

my $want = 'X::Comp::AdHoc: Missing required term after infix';
for '1 and', '1 or', '1 xor', '1 &&', '1 ||', '1 ^^', '1 andthen', '1 orelse',
    '1 Z', '1 X', '1 ...', '1 =', '1 =>' -> $code {
    is failure($code), $want, "'$code' with nothing after it";
}

is failure('(1 or)'), $want, 'before a closing bracket';
