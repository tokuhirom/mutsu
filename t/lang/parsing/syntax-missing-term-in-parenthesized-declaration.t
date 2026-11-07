use Test;

plan 9;

# A declaration inside parentheses or an argument list that ends in an infix
# with no term (`(my $a = 42 *)`) reports rakudo's "Missing required term after
# infix", not a generic parse failure at the `(` (Text::CodeProcessing wraps a
# REPL chunk as `my \_1 = $(⏎ chunk ⏎);`).
sub failure($code) {
    try { EVAL $code; CATCH { default { return .^name ~ ': ' ~ .message } } }
    'no error';
}

my $want = 'X::Comp::AdHoc: Missing required term after infix';
is failure('(my $a = 42 *)'), $want, 'my with = in parens';
is failure('(my @a = 42 +)'), $want, 'my array with = in parens';
is failure('(our $a = 42 *)'), $want, 'our in parens';
is failure('(my $a := 42 *)'), $want, 'my with := in parens';
is failure('(1, my $a = 42 *)'), $want, 'after a comma';
is failure("my \\_1 = \$(\n    my \$answer = 42 *\n);\n"), $want, 'wrapped in $( ) over several lines';
is failure('sub f(*@a) { }; f(my $a = 42 *)'), $want, 'as a call argument';

# Valid declarations in expression position are unaffected.
is failure('(my $a = 42 * 2)'), 'no error', 'a complete declaration in parens';
is failure('(my $b = 1, my $c = 2).elems'), 'no error', 'declarations in a list';

done-testing;
