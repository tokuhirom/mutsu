use Test;

plan 9;

# A line break does not separate the items of `( )` / `$( )` (#16975 left the
# parenthesized forms silent): a plain term on the next line with no `,` or `;`
# is rakudo's "Two terms in a row across lines".
sub failure($code) {
    try { EVAL $code; CATCH { default { return .^name ~ ': ' ~ .message } } }
    'no error';
}

my $want = 'X::Syntax::Confused: Two terms in a row across lines (missing semicolon or comma?)';
is failure("my \$a = 2; (\n\$a ** 3\n\$a ** 2\n)"), $want, 'a parenthesized list';
is failure("my \$a = 2; \$(\n\$a ** 3\n\$a ** 2 ** 3\n)"), $want, 'an itemized parenthesized block';
is failure("say (1\n2\n)"), $want, 'numbers on two lines';
is failure("my \$a = 1; say (\$a\n \$a)"), $want, 'variables on two lines';

# Separators and complete expressions are unaffected.
is failure("my \$a = 2; (1,\n\$a\n)"), 'no error', 'a comma at the end of the line';
is failure("my \$a = 2; (1\n,\$a\n)"), 'no error', 'a comma at the start of the next line';
is failure("my \$a = 2; (\n\$a + 1\n)"), 'no error', 'one expression over several lines';
is failure("my \$a = 2; \$(\n\$a;\n\$a + 1\n)"), 'no error', 'a semicolon between statements';
is failure("my \$a = 2; \$(my \$b = 1;\n\$b + \$a\n)"), 'no error', 'a declaration then a statement';

done-testing;
