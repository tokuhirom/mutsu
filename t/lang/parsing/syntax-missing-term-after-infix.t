use Test;

plan 10;

# An infix operator with no term after it fails with rakudo's
# X::Comp::AdHoc "Missing required term after infix" (#16976).
sub failure($code) {
    try { EVAL $code; CATCH { default { return .^name ~ ': ' ~ .message } } }
    'no error';
}

my $want = 'X::Comp::AdHoc: Missing required term after infix';
is failure("my \$a = 42 *\n"), $want, 'trailing * before a newline';
is failure('1 +'), $want, 'trailing + at the end of input';
is failure('my $x = 1 +;'), $want, 'operator before ;';
is failure('my $x = (1 +)'), $want, 'operator before )';
is failure("my \$a = 2 ** \n"), $want, 'trailing ** with spaces';
is failure('1 xx'), $want, 'word operator xx';
is failure('1 ==' ~ "\n}"), $want, 'comparison before a closing brace';

# Valid code is unaffected.
is failure('my $x = 1 + 2; $x'), 'no error', 'a complete expression';
is failure('my @a = 1, 2,'), 'no error', 'a trailing comma is allowed';
is failure("my \$y = 1 +\n2"), 'no error', 'the operand may be on the next line';
