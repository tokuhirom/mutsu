use Test;

plan 9;

# A single-line expression statement with no `;` that is followed, on the next
# line, by a plain term is not two statements: rakudo reports
# X::Syntax::Confused "Two terms in a row across lines" (#16975).
sub confusion($code) {
    try { EVAL $code; CATCH { when X::Syntax::Confused { return .message } } }
    'no error';
}

my $msg = 'Two terms in a row across lines (missing semicolon or comma?)';
is confusion("my \$a = 1;\n\$a ** 3\n\$a ** 2 ** 3\n"), $msg, 'variable after expression';
is confusion("my \$a = 1;\n\$a + 1\n42\n"), $msg, 'number on the next line';
is confusion("my \$a = 1;\n\$a + 1\n'str'\n"), $msg, 'string on the next line';
is confusion("my \@a = 1, 2;\n\@a.elems\n\@a[0]\n"), $msg, 'array variable on the next line';

# Legal spellings stay legal.
is confusion("my \$a = 1;\n\$a ** 3;\n\$a ** 2 ** 3\n"), 'no error', 'a semicolon separates them';
is confusion('my $a = 1;' ~ "\n" ~ 'if $a { 1 }' ~ "\n" ~ '$a + 2' ~ "\n"), 'no error', 'a closing brace ends a block statement';
is confusion("my \$a = 1\n+ 2\n"), 'no error', 'an infix on the next line continues the expression';
is confusion("my \@a = 1, 2;\n\@a\n.elems\n"), 'no error', 'a method call on the next line continues it';
is confusion("my \$x = 5;\n\$x\n"), 'no error', 'a single statement';
