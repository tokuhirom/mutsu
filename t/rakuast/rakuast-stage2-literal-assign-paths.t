use Test;

# ADR-15334 section 3.1 item 7: the same snippets as a file, as an EVAL string
# and as EVAL of the string's .AST must mean the same thing. Family: assigning
# to an immutable literal (X::Assignment::RO after the right-hand side ran).

my @cases =
    'int',  'my $s = ""; try { 120 = ($s ~= "rhs"; 2); CATCH { default { $s ~= .^name } } }; $s', 'rhsX::Assignment::RO',
    'str',  'my $s = ""; try { "a" = 3; CATCH { default { $s = .message } } }; $s',               'Cannot modify an immutable Str (a)',
    'num',  'my $s = ""; try { 1e0 = 3; CATCH { default { $s = .message } } }; $s',              'Cannot modify an immutable Num (1)',
    'rat',  'my $s = ""; try { 1.5 = 3; CATCH { default { $s = .message } } }; $s',              'Cannot modify an immutable Rat (1.5)';

sub as-file(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', "say do \{ $code }", :out, :err;
    $proc.out.slurp(:close).trim;
}

for @cases -> $name, $code, $expected {
    is as-file($code), $expected, "$name: as a file";
    is EVAL($code).Str, $expected, "$name: as an EVAL string";
    is EVAL($code.AST).Str, $expected, "$name: as EVAL of the string's .AST";
}

done-testing;
